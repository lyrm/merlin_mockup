type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable msg : 'a option }

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
let protect a = Mutex.protect a.mutex
