open Moconfig

type result = (string * int) list ref

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

let res : result = ref []

(* let type_structure shared env ~until defs =
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
                  Obj.magic_uncontended res :=
                    (v, e) :: !(Obj.magic_uncontended res);
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
  loop until env 0 defs *)
let type_structure shared env ~until defs = assert false

let type_implementation handler shared defs config =
  match config.completion with
  | All -> type_structure ~until:Complete shared [] defs
  | Part i ->
      let partial, env, rest =
        type_structure ~until:(Partial i) shared [] defs
      in
      Eff.perform handler (Partial (TI partial));
      let _suffix =
        try type_structure ~until:Complete shared env rest with
        | Cancel_or_closing -> raise Cancel_or_closing
        | exn -> raise (Exception_after_partial exn)
      in
      ()

let run (handler : Eff.Handler.t @ local) shared defs config =
  (* Reset "typer" state *)
  Obj.magic_uncontended res := [];
  let rec handle = function
    | Eff.Value res -> res
    | Exception exn -> raise exn
    | Operation (Partial (TI r), k) ->
        Eff.perform handler (Partial (Run r));
        handle (Effect.continue k () [])
    | Operation (Partial (Run _), _) -> assert false
  in
  handle
    (Eff.run (fun handler -> type_implementation handler shared defs config))
  [@nontail]
