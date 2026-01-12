(* open Effect.Deep
open Moconfig
open Motyper *)

type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Moparser.expr) list;
  evals : Motyper.result;
}
(* 
(** [closing]: called by the main domain *)
let close_typer shared =
  Utils.log 1 "In close_typer";
  Moshared.put_ack shared.msg (Msg `Closing);
  Utils.log 1 "Out of close_typer"

(** [share_exn]: called by the typer domain *)
let share_exn shared exn =
  Utils.log 1 "In shared_exn";
  Moshared.put_ack shared.msg (Msg (`Exn exn));
  Utils.log 1 "Out of shared_exn"

(** [cancel]: called by the main domain *)
let cancel_typer shared =
  Utils.log 1 "In cancel_typer";
  Moshared.put_ack shared.msg (Msg `Cancel);
  Utils.log 1 "Out of cancel_typer"

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
                  Utils.log 1 "Sharing partial result";
                  Moshared.put_ack shared.msg (Partial evals);
                  Utils.log 1 "Shared partial result!";
                  continue k ())
          | _ -> None);
    }

let make config shared = process config shared

(** [domain_typer] *)
let domain_typer shared =
  let rec loop () =
    Utils.log 1 "Looping";
    try
      match Moshared.take shared.msg with
      | Msg `Closing ->
          Utils.log 0 "Closing";
          (* Stopping ! *)
          ()
      | Msg `Cancel ->
          Utils.log 0 "Cancelling";
          loop ()
      | Config config ->
          Utils.log 0 "Beginning new config";
          let pipeline = make config shared in
          (match config.completion with
          | All -> Moshared.put_ack shared.msg (Partial pipeline)
          | _ -> (* Already shared *) ());
          loop ()
      | _ -> failwith "unexpected message in domain_typer"
    with
    | Cancel_or_closing ->
        Utils.log 0 "Caught Cancel_or_closing.";
        loop ()
    | Exception_after_partial exn ->
        let exn = Printexc.to_string exn in
        Utils.log 0 "Caught an exception after partial result : %s." exn;
        loop ()
    | exn ->
        share_exn shared exn;
        loop ()
  in
  loop ()

let get shared config =
  Moshared.put_ack shared.msg (Config config);

  Utils.log 0 "Config changed and received";

  match Moshared.take shared.msg with
  | Partial pipeline ->
      Utils.log 0 "Got partial result";

      let pipeline =
        { source = config.source; raw_def = []; defs = []; evals = pipeline }
      in
      Some pipeline
  | Msg (`Exn exn) ->
      Utils.log 0 "Got exception: %s" (Printexc.to_string exn);
      raise exn
  | _ -> failwith "Unexpected message" *)
