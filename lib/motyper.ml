open Debug

(* open Modomain_msg *)
open Moshared
open Effect
open Effect.Deep
open Moconfig

type result = (string * int) list ref

exception Cancel_exn
exception Closing_exn
exception Exception_after_partial

type msg =
  | Msg of [ `Closing | `Cancel | `Exn of exn ]
  | Config of config
  | Partial of result

type shared = { waiting : bool Atomic.t; msg : msg Shared.t }

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
    if debug_lvl > 1 then
      Format.printf "%sTyping defs %d / %d\n%!" (Utils.domain_name ()) count
        (List.length defs);

    if Atomic.get waiting then (
      if debug_lvl > 0 then
        Format.printf "%sHave read waiting \n%!" (Utils.domain_name ());

      Shared.protect shared_result (fun () ->
          Shared.signal shared_result;
          Shared.wait shared_result));

    (* Should use protect here *)
    Shared.lock shared_result;
    match Shared.unsafe_get shared_result with
    | Some (Msg `Closing) ->
        if debug_lvl > 0 then
          Format.printf "%sClosing in typer \n%!" (Utils.domain_name ());
        Shared.unlock shared_result;
        raise Closing_exn
    | Some (Msg `Cancel) ->
        if debug_lvl > 0 then
          Format.printf "%sCancelling in typer \n%!" (Utils.domain_name ());
        Shared.unlock shared_result;
        raise Cancel_exn
    | None -> (
        match ldefs with
        | def :: rest -> (
            let v, e =
              try Motool_parser.eval env def
              with exn ->
                if debug_lvl > 2 then
                  Format.printf "%sRaising an exception in typer \n%!"
                    (Utils.domain_name ());
                Shared.unlock shared_result;
                raise exn
            in
            res := (v, e) :: !res;
            Shared.unlock shared_result;
            match until with
            | Partial i when i = count -> (res, env, rest)
            | _ ->
                Utils.stupid_work () |> ignore;
                loop until ((v, e) :: env) (count + 1) rest)
        | [] -> (
            if debug_lvl > 0 then
              Format.printf "%sCompution finished \n%!" (Utils.domain_name ());
            Shared.unlock shared_result;
            match until with Partial _ -> (res, env, []) | Complete -> res))
    | Some (Config _) ->
        if debug_lvl > 2 then
          Format.printf "%sUnexpected message in type_structure : config\n%!"
            (Utils.domain_name ());
        Shared.unlock shared_result;
        failwith "Unexpected message in type_structure : config"
    | Some (Partial _) ->
        if debug_lvl > 2 then
          Format.printf "%sUnexpected message in type_structure : partial]\n%!"
            (Utils.domain_name ());
        Shared.unlock shared_result;
        failwith "Unexpected message in type_structure : partial"
    | Some (Msg (`Exn _)) ->
        if debug_lvl > 2 then
          Format.printf "%sUnexpected message in type_structure : exn\n%!"
            (Utils.domain_name ());
        Shared.unlock shared_result;
        failwith "Unexpected message in type_structure : exn"
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
        try type_structure ~until:Complete msg shared_result env rest
        with _ -> raise Exception_after_partial
      in
      res

let run msg shared_result defs config =
  (* Reset "typer" state *)
  res := [];
  match type_implementation msg shared_result defs config with
  | res -> res
  | effect Partial (TI r), k ->
      perform (Partial (Run r));
      continue k ()
