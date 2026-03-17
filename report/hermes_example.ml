(* type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable msg : 'a option }

let create () =
  { mutex = Mutex.create (); cond = Condition.create (); msg = None }

let send_and_wait t msg =
  Mutex.protect t.mutex @@ fun () ->
  assert (t.msg = None);
  let new_v = Some msg in
  t.msg <- new_v;
  Condition.signal t.cond;
  while t.msg == new_v do
    Condition.wait t.cond t.mutex
  done

let recv_clear t =
  Mutex.protect t.mutex @@ fun () ->
  let rec loop () =
    match t.msg with
    | None ->
        Condition.wait t.cond t.mutex;
        loop ()
    | Some v -> v
  in
  let res = loop () in
  t.msg <- None;
  Condition.signal t.cond;
  res

let unsafe_get t = t.msg
let wait a = Condition.wait a.cond a.mutex
let signal a = Condition.signal a.cond
let protect a = Mutex.protect a.mutex *)
open Await
module Lock = Capsule.Mutex.Create ()

type k = Lock.k
type mutex = k Mutex.t

type msg =
  | Msg of [ `Cancel | `Closing | `Exn of exn ] @@ many portable
  | Empty

(* 
type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable msg : msg }

let create () =
  { mutex = Mutex.create (); cond = Condition.create (); msg = None }

let send_and_wait t new_msg =
  Mutex.protect t.mutex (fun () ->
    t.msg <- new_msg;
    Condition.signal t.cond;
    while t.msg == new_msg do
      Condition.wait t.cond t.mutex
    done) 
*)

type 'a t : value mod contended portable = {
  mutex : k Mutex.t;
  cond : k Mutex.Condition.t;
  msg : (msg ref, k) Capsule.Data.t;
}

let create f =
  {
    msg = Capsule.Data.create (fun () -> ref Empty);
    mutex = Lock.mutex;
    cond = Mutex.Condition.create ();
  }

let send_and_wait t new_msg =
  let await = Await_blocking.await Terminator.never in
  Mutex.with_key await t.mutex ~f:(fun key ->
      let #((), key) =
        Capsule.Expert.Key.access key ~f:(fun access ->
            let value = Capsule.Data.unwrap ~access t.msg in
            value := new_msg)
      in
      Mutex.Condition.signal t.cond;
      let key = Mutex.Condition.wait await t.cond ~lock:t.mutex key in
      let rec loop key =
        let #(msg, key) =
          Capsule.Expert.Key.access key ~f:(fun access ->
              { Modes.Aliased.aliased = !(Capsule.Data.unwrap ~access t.msg) })
        in
        if msg.aliased == new_msg then
          let key = Mutex.Condition.wait await t.cond ~lock:t.mutex key in
          loop key
        else #((), key)
      in
      loop key [@nontail])
