open Moconfig

[@@@warning "-37-32"]

type parsedtree = (string * Moparser.expr) list
type typedtree = (string * int) list ref
type result = { config : Moconfig.t; typedtree : typedtree }

exception Cancel_or_closing
exception Exception_after_partial of exn

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = TI of result | Run of result
type _ eff = Partial : partial -> unit eff

module Eff = Effect.Make (struct
  type 'a t = 'a eff
end)

type _ res =
  | Partial :
      int
      -> (result * (string * int) list * (string * Moparser.expr) list) res
  | Complete : result res

let res : (typedtree, Shared.k) Capsule.Data.t =
  Capsule.Data.create (fun () -> ref [])

type state =
  | Finish
  | Rest of (string * Moparser_wrapper.expr) list * (string * int)

let type_structure (shared : typedtree Shared.t) env ~until parsedtree =
  let rec loop until env count ldefs =
    let typer_state =
      Shared.apply shared ~f:(fun msg res ->
          match (msg, ldefs) with
          | Some (Msg `Closing), _ -> raise Cancel_or_closing
          | Some (Msg `Cancel), _ -> raise Cancel_or_closing
          | Some (Config _), _ ->
              failwith "Unexpected message in type_structure : config"
          | Some Partial_is_available, _ ->
              failwith "Unexpected message in type_structure : partial"
          | Some (Msg (`Exn _)), _ ->
              failwith "Unexpected message in type_structure : exn"
          | None, def :: rest ->
              let v, e = Moparser_wrapper.eval_item env def in
              prerr_endline "eval current item";
              prerr_endline "add evaluated items";
              res := (v, e) :: !res;
              Rest (rest, (v, e))
          | None, [] -> Finish)
    in
    match typer_state with
    | Finish ->
        prerr_endline "finish";
        (* (match until with Partial _ -> (res, env, []) | Complete -> res) *)
        ()
    | Rest (rest, (v, e)) ->
        List.init 50 (fun _ -> Random.int 100)
        |> List.fold_left ( + ) 0 |> ignore;
        prerr_endline "loop";
        loop until ((v, e) :: env) (count + 1) rest
    (* (match until with
        | Partial i when i = count -> (res, (v, e) :: env, rest)
        | _ ->
            Utils.stupid_work () |> ignore;
            loop until ((v, e) :: env) (count + 1) rest) *)
  in
  loop until env 0 parsedtree

let type_implementation config shared parsedtree =
  match config.completion with
  | All -> type_structure ~until:Complete shared [] parsedtree
  | Part _ -> assert false

let reset_typer_state () = Shared.protect_capsule res ~f:(fun res -> res := [])

let run config shared parsedtree =
  reset_typer_state ();
  let typedtree = Shared.create_from shared res ~f:(fun x -> x) in
  type_implementation config typedtree parsedtree;
  Shared.map typedtree ~f:(fun typedtree -> { config; typedtree })
