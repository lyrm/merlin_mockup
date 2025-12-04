open Debug

module Shared = struct
  type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a option }

  let create () =
    { mutex = Mutex.create (); cond = Condition.create (); value = None }

  let put_ack t a =
    if debug_lvl > 2 then Utils.log "Blocking_put : waiting for the lock";

    Mutex.lock t.mutex;

    if debug_lvl > 2 then Utils.log "Blocking_put : in CS";

    assert (t.value = None);
    t.value <- Some a;
    Condition.signal t.cond;
    Condition.wait t.cond t.mutex;
    Mutex.unlock t.mutex;

    if debug_lvl > 2 then Utils.log "Blocking_put : after unlock"

  let take t =
    if debug_lvl > 2 then Utils.log "Blocking_take : waiting for the lock";

    Mutex.lock t.mutex;

    if debug_lvl > 2 then Utils.log "Blocking_take : in CS";

    let rec loop () =
      match t.value with
      | None ->
          if debug_lvl > 2 then Utils.log "Blocking_take : ready to wait";

          Condition.wait t.cond t.mutex;
          loop ()
      | Some v -> v
    in
    let res = loop () in
    if debug_lvl > 2 then Utils.log "Blocking_take : got the value";

    t.value <- None;
    Condition.signal t.cond;
    Mutex.unlock t.mutex;

    if debug_lvl > 2 then Utils.log "Blocking_take : after unlock";
    res

  let unsafe_get t = t.value
  let wait a = Condition.wait a.cond a.mutex
  let signal a = Condition.signal a.cond
  let protect a = Mutex.protect a.mutex
  let lock a = Mutex.lock a.mutex
  let unlock a = Mutex.unlock a.mutex
end
