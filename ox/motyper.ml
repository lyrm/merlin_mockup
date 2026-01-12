(* open Effect
open Effect.Deep 
open Moconfig *)

type result = (string * int) list ref

(* exception Cancel_or_closing
exception Exception_after_partial of exn

type msg =
  | Msg of [ `Closing | `Cancel | `Exn of exn ]
  | Config of config
  | Partial of result

type shared = { waiting : bool Atomic.t; msg : msg Moshared.t }

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = TI of result | Run of result
type _ Effect.t += Partial : partial -> unit t

let res : result = ref []

type _ res =
  | Partial :
      int
      -> (result * (string * int) list * (string * Motool_parser.expr) list) res
  | Complete : result res

let type_structure waiting shared_result env ~until defs =
  let rec loop : type r.
      r res ->
      (string * int) list ->
      int ->
      (string * Motool_parser.expr) list ->
      r =
   fun until env count ldefs ->
    Utils.log 1 "Typing defs %d / %d" count (List.length defs);

    if Atomic.get waiting then begin
      Utils.log 0 "Typer domain: detected main domain is waiting";

      Moshared.protect shared_result (fun () ->
          Moshared.signal shared_result;
          Moshared.wait shared_result)
    end;

    (* Should use protect here *)
    Moshared.lock shared_result;
    match Moshared.unsafe_get shared_result with
    | Some (Msg `Closing) ->
        Moshared.unlock shared_result;
        raise Cancel_or_closing
    | Some (Msg `Cancel) ->
        Moshared.unlock shared_result;
        raise Cancel_or_closing
    | Some (Config _) ->
        Moshared.unlock shared_result;
        failwith "Unexpected message in type_structure : config"
    | Some (Partial _) ->
        Moshared.unlock shared_result;
        failwith "Unexpected message in type_structure : partial"
    | Some (Msg (`Exn _)) ->
        Moshared.unlock shared_result;
        failwith "Unexpected message in type_structure : exn"
    | None -> (
        match ldefs with
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
            match until with Partial _ -> (res, env, []) | Complete -> res))
  in
  loop until env 0 defs

let type_implementation msg shared_result defs config =
  match config.completion with
  | All -> type_structure ~until:Complete msg shared_result [] defs
  | Part i ->
      let partial, env, rest =
        type_structure ~until:(Partial i) msg shared_result [] defs
      in
      perform (Partial (TI partial));
      let _suffix =
        try type_structure ~until:Complete msg shared_result env rest with
        | Cancel_or_closing -> raise Cancel_or_closing
        | exn -> raise (Exception_after_partial exn)
      in
      res

let run msg shared_result defs config =
  (* Reset "typer" state *)
  res := [];
  match_with
    (type_implementation msg shared_result defs)
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
                  perform (Partial (Run r));
                  continue k ())
          | _ -> None);
    } *)
