open Await

type msg = Msg of string | Config of int

type 'k unpacked = {
  msg : (msg option ref, 'k) Capsule.Data.t;
  data : (Mopipeline.t option, 'k) Capsule.Data.t;
  mutex : 'k Mutex.t;
  cond : 'k Mutex.Condition.t;
}

type 'a t = P : 'k unpacked -> 'a t

let create () =
  let (P { data; mutex }) =
    Capsule.With_mutex.create (fun () -> (ref None, None))
  in
  P
    {
      msg = Capsule.Data.fst data;
      data = Capsule.Data.snd data;
      mutex;
      cond = Mutex.Condition.create ();
    }

let send_and_wait (P t) msg =
  Await_blocking.with_await Terminator.never ~f:(fun await ->
      Mutex.with_key await t.mutex ~f:(fun key ->
          let #((), key) =
            Capsule.Expert.Key.access key ~f:(fun access ->
                let value = Capsule.Data.unwrap ~access t.msg in
                value := Some msg)
          in
          Mutex.Condition.signal t.cond;
          let key = Mutex.Condition.wait await t.cond ~lock:t.mutex key in
          Capsule.Expert.Key.access key ~f:(fun access ->
              assert (!(Capsule.Data.unwrap ~access t.msg) = None)))
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
                         !(Capsule.Data.unwrap ~access t.msg);
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
                 Capsule.Data.unwrap ~access t.msg := None)
           in
           Mutex.Condition.signal t.cond;
           #(value, key)))
        .aliased)

