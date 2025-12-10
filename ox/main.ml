open Await

type msg = Msg of string | Config of int

module Shared = struct
  type ('a : immutable_data, 'k) t = {
    data : (msg option ref, 'k) Capsule.Data.t;
    mutex : 'k Mutex.t;
    cond : 'k Mutex.Condition.t;
  }

  type 'a packed = P : ('a, 'k) t -> 'a packed

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

  (* let take (P t) =
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
        [@nontail]) *)

  (* let recv_clear (P t) =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        let mutex = Capsule.With_mutex.P { data = t.data; mutex = t.mutex } in
        Capsule.With_mutex.with_lock await mutex ~f:(fun value ->
            let v = !value in
            value := None;
            Mutex.Condition.signal t.cond;
            v)) *)

  let recv_clear (P t) =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            let #(value, key) =
              Capsule.Expert.Key.access key ~f:(fun access ->
                  !(Capsule.Data.unwrap ~access t.data))
            in
            let #(value, key) =
              match value with
              | None ->
                  let key =
                    Mutex.Condition.wait await t.cond ~lock:t.mutex key
                  in
                  let #(value, key) =
                    Capsule.Expert.Key.access key ~f:(fun access ->
                        !(Capsule.Data.unwrap ~access t.data))
                  in
                  Option.get value (* Temporary *)
              | Some v -> v
            in
            Capsule.Data.unwrap ~access t.data := None;
            Mutex.Condition.signal t.cond;
            #(value, key))
        [@nontail])

  (* let recv_clear (P t) =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            (* let value = Capsule.Expert.Data.project t.data in *)
            let #(value, key) =
              Capsule.Expert.Key.access key ~f:(fun access ->
                  let v = Capsule.Data.unwrap ~access t.data in
                  v := None;
                  !v
                  )
            in
            Mutex.Condition.signal t.cond;
            #(value, key))
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
