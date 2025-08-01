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
  if debug_lvl > 1 then
    Format.printf "%sIn close_typer\n%!" (Utils.domain_name ());
  Shared.put_ack shared.msg (Msg `Closing);
  if debug_lvl > 1 then
    Format.printf "%sOut of close_typer\n%!" (Utils.domain_name ())

(** [share_exn]: called by the typer domain *)
let share_exn shared exn =
  if debug_lvl > 1 then
    Format.printf "%sIn shared_exn\n%!" (Utils.domain_name ());
  Shared.put_ack shared.msg (Msg (`Exn exn));
  if debug_lvl > 1 then
    Format.printf "%sOut of shared_exn\n%!" (Utils.domain_name ())

(** [cancel]: called by the main domain *)
let cancel_typer shared =
  if debug_lvl > 1 then
    Format.printf "%sIn cancel_typer\n%!" (Utils.domain_name ());
  Shared.put_ack shared.msg (Msg `Cancel);
  if debug_lvl > 1 then
    Format.printf "%sOut of cancel_typer\n%!" (Utils.domain_name ())

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
      if debug_lvl > 1 then
        Format.printf "%sSharing partial result\n%!" (Utils.domain_name ());
      Shared.put_ack shared.msg (Partial evals);
      if debug_lvl > 1 then Format.printf "%sShared!\n%!" (Utils.domain_name ());
      continue k ()

let make config shared = process config shared

(** [domain_typer] *)
let domain_typer shared () =
  let rec loop () =
    if debug_lvl > 1 then Format.printf "%sLooping\n%!" (Utils.domain_name ());

    try
      match Shared.take shared.msg with
      | Msg `Closing ->
          if debug_lvl > 0 then
            Format.printf "%sClosing\n%!" (Utils.domain_name ());
          (* Stopping ! *)
          ()
      | Msg `Cancel ->
          if debug_lvl > 0 then
            Format.printf "%sCancelling\n%!" (Utils.domain_name ());
          loop ()
      | Config config ->
          if debug_lvl > 0 then
            Format.printf "%sBeginning new config\n%!" (Utils.domain_name ());

          let pipeline = make config shared in
          (match config.completion with
          | All -> Shared.put_ack shared.msg (Partial pipeline)
          | _ -> (* Already shared *) ());
          loop ()
      | _ -> failwith "unexpected message in domain_typer"
    with
    | Cancel_or_closing ->
        if debug_lvl > 0 then
          Format.printf "%sCaught Cancel_or_closing.\n%!" (Utils.domain_name ());
        loop ()
    | Exception_after_partial exn ->
        if debug_lvl > 0 then (
          let exc = Printexc.to_string exn in
          Format.printf "%sCaught an exception after partial result : %s.\n%!"
            (Utils.domain_name ()) exc;
          loop ())
    | exn ->
        share_exn shared exn;
        loop ()
  in

  loop ()

(* let domain_typer shared () =
  let rec loop () =
    if debug_lvl > 1 then Format.printf "%sLooping\n%!" (Utils.domain_name ());

    match Shared.blocking_take shared.msg with
    | Msg `Closing ->
        if debug_lvl > 0 then
          Format.printf "%sClosing\n%!" (Utils.domain_name ());
        (* Stopping ! *)
        ()
    | Msg `Cancel ->
        if debug_lvl > 0 then
          Format.printf "%sCancelling\n%!" (Utils.domain_name ());
        loop ()
    | Config _config ->
        if debug_lvl > 0 then
          Format.printf "%sBeginning new config\n%!" (Utils.domain_name ());

        let pipeline = ref [] in
        (* make config shared in *)
        Shared.blocking_put shared.msg (Partial pipeline);
        loop ()
    | _ -> failwith "unexpected message in domain_typer"
  in

  loop () *)

let get shared config =
  Shared.put_ack shared.msg (Config config);

  if debug_lvl > 0 then
    Format.printf "%sConfig changed and received\n%!" (Utils.domain_name ());

  match Shared.take shared.msg with
  | Partial pipeline ->
      if debug_lvl > 0 then
        Format.printf "%sGot partial result\n%!" (Utils.domain_name ());
      let pipeline =
        { source = config.source; raw_def = []; defs = []; evals = pipeline }
      in
      Some pipeline
  | Msg (`Exn exn) ->
      if debug_lvl > 0 then
        Format.printf "%sGot exception: %s\n%!" (Utils.domain_name ())
          (Printexc.to_string exn);
      raise exn
  | _ -> failwith "Unexpected message"
