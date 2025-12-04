type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a option }

let create () =
  { mutex = Mutex.create (); cond = Condition.create (); value = None }

let put_ack t a =
  Mutex.protect t.mutex @@ fun () ->
  assert (t.value = None);
  t.value <- Some a;
  Condition.signal t.cond;
  Condition.wait t.cond t.mutex

let take t =
  Mutex.protect t.mutex @@ fun () ->
  let rec loop () =
    match t.value with
    | None ->
        Condition.wait t.cond t.mutex;
        loop ()
    | Some v -> v
  in
  let res = loop () in
  t.value <- None;
  Condition.signal t.cond;
  res

let unsafe_get t = t.value
let wait a = Condition.wait a.cond a.mutex
let signal a = Condition.signal a.cond
let protect a = Mutex.protect a.mutex
let lock a = Mutex.lock a.mutex
let unlock a = Mutex.unlock a.mutex
