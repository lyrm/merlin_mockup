open Debug
open Modomain_msg
open Moshared
open Effect
open Effect.Deep
open Moconfig

type result = (string * int) list ref

(* In mtyper.ml, type_structure, type_implementation and run returns different types*)
type partial = TI of result | Run of result
type _ Effect.t += Partial : partial -> unit t

let res : result = ref []

type _ res =
  | Partial :
      int
      -> (result * (string * int) list * (string * Motool_parser.expr) list) res
  | Complete : result res

let type_structure msg shared_result env ~until defs =
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
    | def :: rest -> (
        let v, e = Motool_parser.eval env def in
        res := (v, e) :: !res;

        Shared.unlock shared_result;
        match until with
        | Partial i when i = count -> (res, env, rest)
        | _ ->
            Utils.stupid_work () |> ignore;
            loop until ((v, e) :: env) (count + 1) rest)
    | [] -> (
        Shared.unlock shared_result;
        match until with Partial _ -> (res, env, []) | Complete -> res)
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
      let _suffix = type_structure ~until:Complete msg shared_result env rest in
      res

let run msg shared_result defs config =
  (* Reset "typer" state *)
  res := [];
  match type_implementation msg shared_result defs config with
  | res -> res
  | effect Partial (TI r), k ->
      perform (Partial (Run r));
      continue k ()
