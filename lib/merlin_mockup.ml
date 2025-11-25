open Debug
open Moshared

(** [run_analysis]*)
let run_analysis partial_pipeline _config =
  let evals = partial_pipeline.Mopipeline.evals in
  let save = !evals in
  Motool_parser.rename evals;
  if print_partial then (
    Format.printf "Partial result:\n%!";
    Motool_parser.print evals);
  partial_pipeline.Mopipeline.evals := save

(** [analysis]*)
let analysis (shared : Motyper.shared) (partial_pipeline : Mopipeline.t) config
    =
  (* Main domain signals it wants the lock  *)
  Atomic.set shared.waiting true;

  (* Main domain waits for the typer domain to finish its analysis *)
  Shared.protect shared.msg (fun () ->
      Atomic.set shared.waiting false;
      run_analysis partial_pipeline config;
      Shared.signal shared.msg)

(** [run] = New_merlin.run ou New_commands.run*)
let run =
  let req_count = ref 0 in
  fun shared config ->
    Mopipeline.cancel_typer shared;
    incr req_count;
    let result = Mopipeline.get shared config in
    if debug_lvl > 0 then
      Format.printf "%sRequest nb %d - Beginning analysis\n%!"
        (Utils.domain_name ()) !req_count;
    Option.iter (fun r -> analysis shared r config) result

(** [main] = Ocaml_merlin_server.main *)
let main () =
  let shared = Mopipeline.create_shared () in
  if debug_lvl > 0 then
    Format.printf "%sSpawning typer\n%!" (Utils.domain_name ());

  let domain_typer = Domain.spawn @@ Mopipeline.domain_typer shared in

  Server.listen ~socket_fname:Sys.argv.(1) ~handle:(fun req ->
      run shared req;
      let res = !Motyper.res |> List.rev in
      Motool_parser.to_string (ref res));
  Mopipeline.close_typer shared;
  Domain.join domain_typer;

  if print_last then (
    Format.printf "Last result:\n%!";
    let res = !Motyper.res |> List.rev in
    Motool_parser.print (ref res));
  ()

(* Clean up the socket once the server is shutdown. *)
let () =
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> Unix.unlink Sys.argv.(1)))

let () = main ()

(*
  Is there an issue depending on when an exception is raised by the typer domain ?

  - before a partial result has been returned : No, the main domain is located 
  before the write on `curr_config` and the wait on `partial_result`.  

  - after a partial result has been returned : the main domain can be anywhere. 
  In particular, it could be already processing a new request / config. However, 
  it only becomes aware of the issue when it looks at the `mess_main`. But then 
  the typer is waiting for ACK to continue. So it should be has soon as possible. 
  -> It is not an issue if the reading of `mess_main` is always able to manage the 
    possible caught exception and if it happens often enough. 

*)

(* Things in Merlin that might not work well
- ctype
- src/analysis
- local_store
- unify_gadt -> in lock (with snapshot /backtrack around )
*)
