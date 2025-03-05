type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a }

let locking_set t a =
  Mutex.protect t.mutex @@ fun () ->
  t.value <- a;
  Condition.signal t.cond

let set t a =
  t.value <- a;
  Condition.signal t.cond

let locking_get t = Mutex.protect t.mutex @@ fun () -> t.value
let get t = t.value

let create a =
  { mutex = Mutex.create (); cond = Condition.create (); value = a }

let wait a = Condition.wait a.cond a.mutex
let signal a = Condition.signal a.cond
let protect a = Mutex.protect a.mutex
let lock a = Mutex.lock a.mutex
let unlock a = Mutex.unlock a.mutex

(* 
 Can I do something that guarantees that the function are called under lock ?
*)
