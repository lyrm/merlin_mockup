open Await

type msg = Msg of int ref | Config of int

module Shared = struct
  type ('a, 'k) t = {
    data : (int option ref, 'k) Capsule.Data.t;
    mutex : 'k Mutex.t;
    cond : 'k Mutex.Condition.t;
  }

  type 'a packed = P : ('a, 'k) t -> 'a packed

  let create () =
    let (P { data; mutex }) = Capsule.With_mutex.create (fun () -> ref None) in
    P { data; mutex; cond = Mutex.Condition.create () }

  let put_ack (P t) msg =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            let #((), key) =
              Capsule.Expert.Key.access key ~f:(fun access ->
                  let value = Capsule.Data.unwrap ~access t.data in
                  value := Some msg)
            in
            Mutex.Condition.signal t.cond;
            let key = Mutex.Condition.wait await t.cond ~lock:t.mutex key in
            #((), key))
        [@nontail])

  let take (P t) =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                let rec loop () =
                  match !(Capsule.Data.unwrap ~access t.data) with
                  | None ->
                      let _key =
                        Mutex.Condition.wait await t.cond ~lock:t.mutex key
                      in
                      loop () [@nontail]
                  | Some v -> v
                in
                let value = loop () in
                Capsule.Data.unwrap ~access t.data := None;
                Mutex.Condition.signal t.cond;
                value)
            [@nontail])
        [@nontail])

  (* let take (P t) =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            let rec loop key =
              let #(value, key) =
                Capsule.Expert.Key.access key ~f:(fun access ->
                    (Capsule.Data.unwrap ~access t.data))
              in
              match value with
              | None ->
                  let key =
                    Mutex.Condition.wait await t.cond ~lock:t.mutex key
                  in
                  loop key [@nontail]
              | Some v -> v
            in
            let value = loop key in
            Capsule.Data.unwrap ~access t.data := None;
            Mutex.Condition.signal t.cond;
            value [@nontail])
        [@nontail]) *)
end

let domain_typer shared =
  match Shared.take shared with
  | Config cfg -> Shared.put_ack shared.msg (Msg (ref (Some cfg)))
  | Msg _ -> failwith "unexpected msg"

let main () =
  let shared @ shared = Shared.create () in
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
