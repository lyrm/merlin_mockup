module Effect = Stdlib.Effect
open Effect.Deep
open Moconfig

type result = (string * int) list ref

exception Cancel_or_closing
exception Exception_after_partial of exn

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = TI of result | Run of result
type _ Effect.t += Partial : partial -> unit Effect.t

type _ res =
  | Partial :
      int
      -> (result * (string * int) list * (string * Moparser.expr) list) res
  | Complete : result res

let res = Obj.magic_uncontended (ref [])

let type_structure shared env ~until defs =
  let rec loop : type r. r res -> _ -> _ -> _ -> r =
   fun until (env : (string * int) list) (count : int)
       (ldefs : (string * Moparser.expr) list) ->
    match
      Shared.protected_apply shared (fun msg _pipeline ->
          match msg with
          | Some (Msg (`Closing | `Cancel)) -> raise Cancel_or_closing
          | Some (Config _ | Partial_is_available | Msg (`Exn _)) ->
              failwith "Unexpected message in type_structure"
          | None -> begin
              match ldefs with
              | def :: rest -> begin
                  let v, e =
                    try Moparser.eval env def with exn -> raise exn
                  in
                  res := (v, e) :: !res;
                  match until with
                  | Partial i when i = count -> (res, (v, e) :: env, rest)
                  | _ ->
                      Utils.stupid_work () |> ignore;
                      ((v, e) :: env, rest)
                end
              | [] -> begin
                  match until with
                  | Partial _ -> (res, env, [])
                  | Complete -> res
                end
            end)
    with
    | `Return x -> x
    | `Loop (env, rest) -> loop until env (count + 1) rest
  in
  loop until env 0 defs

let type_implementation shared defs config =
  match config.completion with
  | All -> type_structure ~until:Complete shared [] defs
  | Part i ->
      let partial, env, rest =
        type_structure ~until:(Partial i) shared [] defs
      in
      Effect.perform (Partial (TI partial));
      let _suffix =
        try type_structure ~until:Complete shared env rest with
        | Cancel_or_closing -> raise Cancel_or_closing
        | exn -> raise (Exception_after_partial exn)
      in
      ()

let run shared defs config =
  (* Reset "typer" state *)
  res := [];
  match_with
    (type_implementation shared defs)
    config
    {
      retc = (fun res -> res);
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Partial (TI r) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.perform (Partial (Run r));
                  continue k ())
          | _ -> None);
    }
