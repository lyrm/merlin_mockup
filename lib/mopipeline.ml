open Debug
open Stdlib.Effect.Deep
open Moconfig
open Motyper

type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Motool_parser.expr) list;
  evals : Motyper.result;
}

(** [closing]: called by the main domain *)
let close_typer shared = Moshared.put_ack shared.msg (Msg `Closing)

(** [share_exn]: called by the typer domain *)
let share_exn shared exn = Moshared.put_ack shared.msg (Msg (`Exn exn))

(** [cancel]: called by the main domain *)
let cancel_typer shared = Moshared.put_ack shared.msg (Msg `Cancel)

(** [create_shared] *)
let create_shared () = { waiting = Atomic.make false; msg = Moshared.create () }

let process config shared =
  let raw_def = Motool_parser.buffer_to_words config.Moconfig.source in
  let defs =
    List.map Motool_parser.lexer raw_def |> List.map Motool_parser.parse_def
  in
  match_with
    (Motyper.run shared.waiting shared.msg defs)
    config
    {
      retc = (fun evals -> evals);
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Motyper.Partial (Run evals) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  if debug_lvl > 1 then
                    Format.printf "%sSharing partial result\n%!"
                      (Utils.domain_name ());
                  Moshared.put_ack shared.msg (Partial evals);
                  if debug_lvl > 1 then
                    Format.printf "%sshared!\n%!" (Utils.domain_name ());
                  continue k ())
          | _ -> None);
    }

let make config shared = process config shared

(** [domain_typer] *)
let domain_typer shared () =
  let rec loop () =
    if debug_lvl > 1 then Format.printf "%sLooping\n%!" (Utils.domain_name ());

    try
      match Moshared.take shared.msg with
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
          | All -> Moshared.put_ack shared.msg (Partial pipeline)
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

    match Moshared.blocking_take shared.msg with
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
        Moshared.blocking_put shared.msg (Partial pipeline);
        loop ()
    | _ -> failwith "unexpected message in domain_typer"
  in

  loop () *)

let get shared config =
  Moshared.put_ack shared.msg (Config config);

  if debug_lvl > 0 then
    Format.printf "%sConfig changed and received\n%!" (Utils.domain_name ());

  match Moshared.take shared.msg with
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
