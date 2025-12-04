open Debug
open Moshared

(* open Modomain_msg *)
open Effect.Deep
open Moconfig
open Motyper

type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Motool_parser.expr) list;
  evals : Motyper.result;
}

(** [closing]: called by the main domain *)
let close_typer shared =
  if debug_lvl > 1 then Utils.log "In close_typer";
  Shared.put_ack shared.msg (Msg `Closing);
  if debug_lvl > 1 then Utils.log "Out of close_typer"

(** [share_exn]: called by the typer domain *)
let share_exn shared exn =
  if debug_lvl > 1 then Utils.log "In shared_exn";
  Shared.put_ack shared.msg (Msg (`Exn exn));
  if debug_lvl > 1 then Utils.log "Out of shared_exn"

(** [cancel]: called by the main domain *)
let cancel_typer shared =
  if debug_lvl > 1 then Utils.log "In cancel_typer";
  Shared.put_ack shared.msg (Msg `Cancel);
  if debug_lvl > 1 then Utils.log "Out of cancel_typer"

(** [create_shared] *)
let create_shared () = { waiting = Atomic.make false; msg = Shared.create () }

let process config shared =
  let raw_def = Motool_parser.buffer_to_words config.Moconfig.source in
  let defs =
    List.map Motool_parser.lexer raw_def |> List.map Motool_parser.parse_def
  in
  match Motyper.run shared.waiting shared.msg defs config with
  | evals -> evals
  | effect Motyper.Partial (Run evals), k ->
      if debug_lvl > 1 then Utils.log "Sharing partial result";
      Shared.put_ack shared.msg (Partial evals);
      if debug_lvl > 1 then Utils.log "Shared!";
      continue k ()

let make config shared = process config shared

(** [domain_typer] *)
let domain_typer shared =
  let rec loop () =
    if debug_lvl > 1 then Utils.log "Looping";

    try
      match Shared.take shared.msg with
      | Msg `Closing ->
          if debug_lvl > 0 then Utils.log "Closing";
          (* Stopping ! *)
          ()
      | Msg `Cancel ->
          if debug_lvl > 0 then Utils.log "Cancelling";
          loop ()
      | Config config ->
          if debug_lvl > 0 then Utils.log "Beginning new config";

          let pipeline = make config shared in
          (match config.completion with
          | All -> Shared.put_ack shared.msg (Partial pipeline)
          | _ -> (* Already shared *) ());
          loop ()
      | _ -> failwith "unexpected message in domain_typer"
    with
    | Cancel_or_closing ->
        if debug_lvl > 0 then Utils.log "Caught Cancel_or_closing.";
        loop ()
    | Exception_after_partial exn ->
        if debug_lvl > 0 then (
          let exn = Printexc.to_string exn in
          Utils.log "Caught an exception after partial result : %s." exn;
          loop ())
    | exn ->
        share_exn shared exn;
        loop ()
  in

  loop ()

(* let domain_typer shared () =
  let rec loop () =
    if debug_lvl > 1 then Utils.log "Looping";

    match Shared.blocking_take shared.msg with
    | Msg `Closing ->
        if debug_lvl > 0 then Utils.log "Closing";
        (* Stopping ! *)
        ()
    | Msg `Cancel ->
        if debug_lvl > 0 then
          Utils.log "Cancelling";
        loop ()
    | Config _config ->
        if debug_lvl > 0 then Utils.log "Beginning new config";
        let pipeline = ref [] in
        (* make config shared in *)
        Shared.blocking_put shared.msg (Partial pipeline);
        loop ()
    | _ -> failwith "unexpected message in domain_typer"
  in

  loop () *)

let get shared config =
  Shared.put_ack shared.msg (Config config);

  if debug_lvl > 0 then Utils.log "Config changed and received";

  match Shared.take shared.msg with
  | Partial pipeline ->
      if debug_lvl > 0 then Utils.log "Got partial result";
      let pipeline =
        { source = config.source; raw_def = []; defs = []; evals = pipeline }
      in
      Some pipeline
  | Msg (`Exn exn) ->
      if debug_lvl > 0 then
        Utils.log "Got exception: %s" (Printexc.to_string exn);
      raise exn
  | _ -> failwith "Unexpected message"
