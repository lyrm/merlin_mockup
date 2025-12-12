open! Await

type msg = Msg of string | Config of int

module Shared = struct
  type 'k t = {
    data : (msg option ref, 'k) Capsule.Data.t;
    mutex : 'k Mutex.t;
    cond : 'k Mutex.Condition.t;
  }

  type 'a packed = P : 'k t -> 'a packed

  let create () =
    let (P { data; mutex }) = Capsule.With_mutex.create (fun () -> ref None) in
    P { data; mutex; cond = Mutex.Condition.create () }

  let send_and_wait (P t) msg =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            let #((), key) =
              Capsule.Expert.Key.access key ~f:(fun access ->
                  let value = Capsule.Data.unwrap ~access t.data in
                  value := Some msg)
            in
            Mutex.Condition.signal t.cond;
            let key = Mutex.Condition.wait await t.cond ~lock:t.mutex key in
            Capsule.Expert.Key.access key ~f:(fun access ->
                assert (!(Capsule.Data.unwrap ~access t.data) = None)))
        [@nontail])

  let recv_clear (P t) =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        (Mutex.with_key await t.mutex ~f:(fun key ->
             let #(value, key) =
               let rec loop key =
                 let #(value, key) =
                   Capsule.Expert.Key.access key ~f:(fun access ->
                       {
                         Modes.Aliased.aliased =
                           !(Capsule.Data.unwrap ~access t.data);
                       })
                 in
                 match value.aliased with
                 | None ->
                     let key =
                       Mutex.Condition.wait await t.cond ~lock:t.mutex key
                     in
                     loop key [@nontail]
                 | Some value -> #({ Modes.Aliased.aliased = value }, key)
               in
               loop key
             in
             let #((), key) =
               Capsule.Expert.Key.access key ~f:(fun access ->
                   Capsule.Data.unwrap ~access t.data := None)
             in
             Mutex.Condition.signal t.cond;
             #(value, key)))
          .aliased)
end

let domain_typer shared =
  match Shared.recv_clear shared with
  | Config cfg -> Shared.send_and_wait shared (Msg "test")
  | Msg _ -> failwith "unexpected msg"

let main () =
  let shared = Shared.create () in
  let module Scheduler = Parallel_scheduler_work_stealing in
  let scheduler = Scheduler.create () in
  Scheduler.parallel scheduler ~f:(fun par ->
      let #((), ()) =
        Parallel.fork_join2 par
          (fun _ -> Mopipeline.domain_typer shared)
          (fun _ -> (* Envoie msg *) ())
      in
      ());
  Scheduler.stop scheduler

let () = main ()
