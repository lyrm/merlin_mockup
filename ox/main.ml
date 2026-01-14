open! Await

let domain_typer shared =
  match Shared.recv_clear shared with
  | Config _cfg ->
      Shared.send_and_wait shared
        (Config { completion = All; source = "some file" })
  | _ -> failwith "unexpected msg"

let main () =
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
  Scheduler.stop scheduler

let () = main ()
