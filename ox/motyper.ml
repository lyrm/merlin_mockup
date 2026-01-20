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

let type_structure _shared env ~until defs =
  let loop : type r.
      r res ->
      (string * int) list ->
      int ->
      (string * Moparser.expr) list ->
      unit =
   fun _until _env _count _ldefs -> ()
   (* Should use protect here *)
   (* Shared.protected_apply shared (fun msg _ ->
        match msg with
        | Some (Msg `Closing) -> raise Cancel_or_closing
        | Some (Msg `Cancel) -> raise Cancel_or_closing
        | Some (Config _) ->
            failwith "Unexpected message in type_structure : config"
        | Some Partial_is_available ->
            failwith "Unexpected message in type_structure : partial"
        | Some (Msg (`Exn _)) ->
            failwith "Unexpected message in type_structure : exn"
        | None -> assert false) *)
   (* match ldefs with
            | def :: rest -> (
                let v, e =
                  try Motool_parser.eval env def
                  with exn ->
                    Moshared.unlock shared_result;
                    raise exn
                in
                res := (v, e) :: !res;
                Moshared.unlock shared_result;
                match until with
                | Partial i when i = count -> (res, (v, e) :: env, rest)
                | _ ->
                    Utils.stupid_work () |> ignore;
                    loop until ((v, e) :: env) (count + 1) rest)
            | [] -> (
                Moshared.unlock shared_result;
                match until with Partial _ -> (res, env, []) | Complete -> res)) *)
  in
  loop until env 0 defs

let type_implementation shared defs config =
  match config.completion with
  | All -> type_structure ~until:Complete shared [] defs
  | Part _ -> assert false

let run shared defs config =
  (* Reset "typer" state *)
  res := [];
  type_implementation shared defs config
