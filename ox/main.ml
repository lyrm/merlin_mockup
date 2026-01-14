open! Await

let domain_typer shared =
  match Shared.recv_clear shared with
  | Config _cfg ->
      Shared.protected_update shared (fun _msg _pipeline ->
          Some
            {
              source = "";
              raw_def = [];
              defs = [];
              evals = ref [];
            });
      Shared.send_and_wait shared Shared.Partial_is_available
  | _ -> failwith "unexpected msg"

(* let main () =
  let shared = Shared.create () in
  let module Scheduler = Parallel_scheduler in
  let scheduler = Scheduler.create () in
  Scheduler.parallel scheduler ~f:(fun par ->
      let #((), ()) =
        Parallel.fork_join2 par
          (fun _ -> domain_typer shared)
          (fun _ -> (* Envoie msg *) ())
      in
      ());
  Scheduler.stop scheduler *)

let get shared config =
  Shared.send_and_wait shared Shared.Partial_is_available;
  match Shared.recv_clear shared with
  | Partial_is_available -> ()
  | Msg (`Exn exn) -> raise exn
  | _ -> failwith "Unexpected message"

let run shared config = get shared config

let main () =
  let shared = Shared.create () in
  let module Scheduler = Parallel_scheduler in
  let scheduler = Scheduler.create () in
  Scheduler.parallel scheduler ~f:(fun par ->
      let #((), ()) =
        Parallel.fork_join2 par
          (fun _ ->
            Server.listen ~socket_fname:Sys.argv.(1) ~handle:(fun req ->
                run shared req;
                let res = !Motyper.res |> List.rev in
                Motool_parser.to_string (ref res));
            Mopipeline.close_typer shared)
          (fun _ -> domain_typer shared)
      in
      ());

  if print_last then begin
    Utils.log 0 "Last result :";
    Motool_parser.print (ref (List.rev !Motyper.res))
  end

let () = main ()
