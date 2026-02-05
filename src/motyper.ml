open Moconfig

type parsedtree = (string * Moparser.expr) list
type typedtree = (string * int) list ref
type result = { config : Moconfig.t; typedtree : typedtree }

exception Cancel_or_closing
exception Exception_after_partial of exn

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = Type_implem | Run of result Hermes.t
type _ eff = Partial : partial -> unit eff

module Eff = Handled_effect.Make (struct
  type 'a t = 'a eff
end)

type env = (string * int) list (* TODO: remove this *)
type _ res = Partial : int -> (env * parsedtree) res | Complete : unit res

let res : (typedtree, Hermes.k) Capsule.Data.t =
  Capsule.Data.create (fun () -> ref [])

type state = Finish | Rest of Moparser.parsedtree * Moparser.typed_item

let type_structure ~until typedtree env parsedtree =
  let rec loop : type a. a res -> env -> int -> parsedtree -> a =
   fun until env count ldefs ->
    Log.debug 1 "Typing defs %d / %d" count (List.length parsedtree);
    let typer_state =
      Hermes.apply typedtree ~f:(fun msg res ->
          match (msg, ldefs) with
          | Msg `Closing, _ -> raise Cancel_or_closing
          | Msg `Cancel, _ -> raise Cancel_or_closing
          | Config _, _ ->
              failwith "Unexpected message in type_structure : config"
          | Partial_is_available, _ ->
              failwith "Unexpected message in type_structure : partial"
          | Msg (`Exn _), _ ->
              failwith "Unexpected message in type_structure : exn"
          | Empty, def :: rest ->
              let v, e = Moparser_wrapper.eval_item env def in
              res := (v, e) :: !res;
              Rest (rest, (v, e))
          | Empty, [] -> Finish)
    in
    match (typer_state, until) with
    | Finish, Partial _ -> (env, [])
    | Finish, Complete -> ()
    | Rest (rest, (v, e)), Partial i when i = count -> ((v, e) :: env, rest)
    | Rest (rest, (v, e)), _ ->
        List.init 50 (fun _ -> Random.int 100)
        |> List.fold_left ( + ) 0 |> ignore;
        loop until ((v, e) :: env) (count + 1) rest
  in
  loop until env 0 parsedtree

let type_implementation config ~handler typedtree parsedtree =
  match config.completion with
  | All -> type_structure ~until:Complete typedtree [] parsedtree
  | Part i -> begin
      let env, rest =
        type_structure ~until:(Partial i) typedtree [] parsedtree
      in
      Eff.perform handler (Partial Type_implem);
      (* Suffix processing: *)
      try type_structure ~until:Complete typedtree env rest with
      | Cancel_or_closing -> raise Cancel_or_closing
      | exn -> raise (Exception_after_partial exn)
    end

let reset_typer_state () = Hermes.protect_capsule res ~f:(fun res -> res := [])

let run config hermes ~(handler @ local) parsedtree =
  reset_typer_state ();
  let result = Hermes.create_from hermes res ~f:Fun.id in
  let rec handle = function
    | Eff.Value x -> x
    | Exception exn -> raise exn
    | Operation (Partial Type_implem, k) ->
        let result =
          Hermes.map result ~f:(fun typedtree -> { config; typedtree })
        in
        Eff.perform handler (Partial (Run result));
        handle (Handled_effect.continue k () [ handler ])
    | Operation (Partial _, k) -> assert false
  in

  handle
    (Eff.run_with [ handler ] (fun [ handler; _ ] ->
         type_implementation config ~handler result parsedtree));
  Hermes.map result ~f:(fun typedtree -> { config; typedtree })
