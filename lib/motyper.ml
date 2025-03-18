open Debug
open Modomain_msg
open Moshared
open Effect
open Effect.Deep
open Moconfig

type result = (string * int) list ref

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = TS of result | TI of result | Run of result
type _ Effect.t += Partial : partial -> unit t

let res : result = ref []

let type_structure msg shared_result env config defs =
  let rec loop env count ldefs =
    if debug_lvl > 1 then
      Format.printf "%sTyping defs %d / %d\n%!" (Utils.domain_name ()) count
        (List.length defs);

    (match Atomic.get msg.Domain_msg.from_main with
    | `Empty -> ()
    | `Waiting ->
        if debug_lvl > 1 then
          Format.printf "%sLetting the main domain get the lock on result\n%!"
            (Utils.domain_name ());
        while Atomic.get msg.Domain_msg.from_main == `Waiting do
          Domain.cpu_relax ()
        done
    | `Closing ->
        if debug_lvl > 0 then
          Format.printf "%sClosing in typer \n%!" (Utils.domain_name ());
        raise Closing
    | `Cancel ->
        if debug_lvl > 0 then
          Format.printf "%sCancelling in typer \n%!" (Utils.domain_name ());
        (* TODO : catching Cancel in Mtyper to cache the already done computation *)
        raise Cancel);

    Shared.lock shared_result;
    match ldefs with
    | def :: rest ->
        let v, e = Motool_parser.eval env def in
        res := (v, e) :: !res;

        Shared.unlock shared_result;
        (* TODO count >= i *)
        (match config.completion with
        | Part i when count = i ->
            if debug_lvl > 0 then
              Format.printf "%sPartial result is ready\n%!"
                (Utils.domain_name ());
            perform (Partial (TS res))
        | _ -> ());

        Utils.stupid_work () |> ignore;
        loop ((v, e) :: env) (count + 1) rest
    | [] ->
        Shared.unlock shared_result;
        res
  in
  loop env 0 defs

let type_implementation msg shared_result defs config =
  match type_structure msg shared_result [] config defs with
  | res -> res
  | effect Partial (TS r), k ->
      perform (Partial (TI r));
      continue k ()

let run msg shared_result defs config =
  (* Reset "typer" state *)
  res := [];
  match type_implementation msg shared_result defs config with
  | res -> res
  | effect Partial (TI r), k ->
      perform (Partial (Run r));
      continue k ()
