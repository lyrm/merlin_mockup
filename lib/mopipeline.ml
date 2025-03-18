open Debug
open Moshared
open Modomain_msg
open Effect.Deep
open Moconfig

type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Motool_parser.expr) list;
  evals : Motyper.result;
}

type shared = {
  msg : Domain_msg.msg;
  config : config option Shared.t;
  (* Partial result *)
  partial : t option Shared.t;
  (* Use to protect typer computation *)
  result : unit Shared.t;
}

(** [closing]: called by the main domain *)
let close_typer shared =
  Domain_msg.send_msg shared.msg.from_main `Closing shared.config

(** [share_exn]: called by the typer domain *)
let share_exn shared exn =
  Domain_msg.send_msg shared.msg.from_typer (`Exn exn) shared.partial

(** [cancel]: called by the main domain *)
let cancel shared =
  Domain_msg.send_msg shared.msg.from_main `Cancel shared.config

(** [create_shared] *)
let create_shared () =
  {
    msg = Domain_msg.create ();
    config = Shared.create None;
    partial = Shared.create None;
    result = Shared.create ();
  }

let process config shared =
  let raw_def = Motool_parser.buffer_to_words config.Moconfig.source in
  let defs =
    List.map Motool_parser.lexer raw_def |> List.map Motool_parser.parse_def
  in
  match Motyper.run shared.msg shared.result defs config with
  | evals -> { source = config.source; raw_def; defs; evals }
  | effect Motyper.Partial (Run evals), k ->
      if debug_lvl > 1 then
        Format.printf "%sSharing partial result\n%!" (Utils.domain_name ());
      let partial_result = { source = config.source; raw_def; defs; evals } in
      Shared.locking_set shared.partial (Some partial_result);
      if debug_lvl > 1 then Format.printf "%sShared!\n%!" (Utils.domain_name ());
      continue k ()

let make config shared = process config shared

(** [domain_typer] *)
let domain_typer shared () =
  let rec loop () =
    if debug_lvl > 1 then Format.printf "%sLooping\n%!" (Utils.domain_name ());

    match Atomic.get shared.msg.from_main with
    | `Closing ->
        if debug_lvl > 0 then
          Format.printf "%sClosing\n%!" (Utils.domain_name ());
        Atomic.set shared.msg.from_main `Empty
    | `Waiting ->
        (* Can't we progress here instead of waiting ? the typer is not even close to requesting the main lock*)
        while Atomic.get shared.msg.from_main == `Waiting do
          Domain.cpu_relax ()
        done;
        loop ()
    | `Cancel ->
        if debug_lvl > 0 then
          Format.printf "%sCancelling\n%!" (Utils.domain_name ());
        Atomic.set shared.msg.from_main `Empty;
        loop ()
    | `Empty -> (
        match Shared.get shared.config with
        | None ->
            if debug_lvl > 0 then
              Format.printf "%sWaiting for config\n%!" (Utils.domain_name ());

            Shared.wait shared.config;
            loop ()
        | Some config ->
            (if debug_lvl > 0 then
               Format.printf "%sBeginning new config\n%!" (Utils.domain_name ());

             Shared.set shared.config None;
             try
               let r = make config shared in
               match config.completion with
               | All -> Shared.locking_set shared.partial (Some r)
               | _ -> (* Already shared *) ()
             with
             | Closing -> ()
             | Cancel -> ()
             | exn -> share_exn shared exn);
            loop ())
  in

  Shared.protect shared.config (fun () -> loop ())

let get shared config =
  Shared.locking_set shared.config (Some config);

  if debug_lvl > 0 then
    Format.printf "%sConfig changed\n%!" (Utils.domain_name ());

  let rec loop () =
    let critical_section () =
      match Shared.get shared.partial with
      | None -> (
          match Atomic.get shared.msg.from_typer with
          | `Empty ->
              if debug_lvl > 0 then
                Format.printf "%sWaiting for result\n%!" (Utils.domain_name ());

              Shared.wait shared.partial;
              `Retry
          | `Exn exn -> failwith (Printexc.to_string exn))
      | Some pipeline ->
          Shared.set shared.partial None;
          `Result (Some pipeline)
    in
    match Shared.protect shared.partial critical_section with
    | `Retry -> loop ()
    | `Result r -> r
  in
  loop ()
