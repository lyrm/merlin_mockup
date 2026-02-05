open Effect
open Effect.Deep
open Moconfig

type parsedtree = (string * Moparser.expr) list
type typedtree = (string * int) list ref
type result = { config : Moconfig.t; typedtree : typedtree }

exception Cancel_or_closing
exception Exception_after_partial of exn

type msg =
  | Msg of [ `Close | `Cancel | `Exn of exn ]
  | Config of Moconfig.t
  | Partial of result

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = Type_implem of typedtree | Run of result
type _ Effect.t += Partial : partial -> unit Effect.t
type env = (string * int) list

type _ res =
  | Partial : int -> (typedtree * env * parsedtree) res
  | Complete : typedtree res

let res : typedtree = ref []

let type_structure ~until typedtree env parsedtree =
  let rec loop : type a. a res -> env -> int -> parsedtree -> a =
   fun until env count ldefs ->
    Utils.log 1 "Typing defs %d / %d" count (List.length ldefs);

    let typer_state =
      Hermes.protect typedtree (fun () ->
          match (Hermes.unsafe_get typedtree, ldefs) with
          | Some (Msg (`Cancel | `Close)), _ -> `Exn Cancel_or_closing
          | Some (Config _), _ ->
              `Exn (Failure "Unexpected message in type_structure : config")
          | Some (Partial _), _ ->
              `Exn (Failure "Unexpected message in type_structure : partial")
          | Some (Msg (`Exn _)), _ ->
              `Exn (Failure "Unexpected message in type_structure : exn")
          | None, def :: rest -> begin
              try
                let v, e = Moparser.eval_item env def in
                res := (v, e) :: !res;
                `Rest (rest, (v, e))
              with exn -> `Exn exn
            end
          | None, [] -> `Finish)
    in
    match (typer_state, until) with
    | `Exn exn, _ -> raise exn
    | `Finish, Partial _ -> (res, env, [])
    | `Finish, Complete -> res
    | `Rest (rest, (v, e)), Partial i when i = count ->
        (res, (v, e) :: env, rest)
    | `Rest (rest, (v, e)), _ ->
        Utils.stupid_work ();
        loop until ((v, e) :: env) (count + 1) rest
  in
  loop until env 0 parsedtree

let type_implementation config typedtree parsedtree =
  match config.completion with
  | All -> type_structure ~until:Complete typedtree [] parsedtree
  | Part i ->
      let partial, env, rest =
        type_structure ~until:(Partial i) typedtree [] parsedtree
      in
      perform (Partial (Type_implem partial));
      (* Suffix processing. *)
      let _ =
        try type_structure ~until:Complete typedtree env rest with
        | Cancel_or_closing -> raise Cancel_or_closing
        | exn -> raise (Exception_after_partial exn)
      in
      res

let run config hermes parsedtree =
  (* Reset "typer" state *)
  res := [];
  try
    let typedtree = type_implementation config hermes parsedtree in
    { config; typedtree }
  with effect Partial (Type_implem typedtree), k ->
    perform (Partial (Run { config; typedtree }));
    continue k ()
