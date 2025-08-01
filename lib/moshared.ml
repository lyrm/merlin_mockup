open Debug

module Shared = struct
  type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a option }

  let create () =
    { mutex = Mutex.create (); cond = Condition.create (); value = None }

  let put_ack t a =
    if debug_lvl > 2 then
      Format.printf "%sBlocking_put : waiting for the lock \n%!"
        (Utils.domain_name ());

    Mutex.lock t.mutex;

    if debug_lvl > 2 then
      Format.printf "%sBlocking_put : in CS \n%!" (Utils.domain_name ());

    assert (t.value = None);
    t.value <- Some a;
    Condition.signal t.cond;
    Condition.wait t.cond t.mutex;
    Mutex.unlock t.mutex;

    if debug_lvl > 2 then
      Format.printf "%sBlocking_put : after unlock \n%!" (Utils.domain_name ())

  let take t =
    if debug_lvl > 2 then
      Format.printf "%sBlocking_take : waiting for the lock \n%!"
        (Utils.domain_name ());

    Mutex.lock t.mutex;

    if debug_lvl > 2 then
      Format.printf "%sBlocking_take : in CS \n%!" (Utils.domain_name ());

    let rec loop () =
      match t.value with
      | None ->
          if debug_lvl > 2 then
            Format.printf "%sBlocking_take : ready to wait \n%!"
              (Utils.domain_name ());

          Condition.wait t.cond t.mutex;
          loop ()
      | Some v -> v
    in
    let res = loop () in
    if debug_lvl > 2 then
      Format.printf "%sBlocking_take : got the value \n%!"
        (Utils.domain_name ());

    t.value <- None;
    Condition.signal t.cond;
    Mutex.unlock t.mutex;

    if debug_lvl > 2 then
      Format.printf "%sBlocking_take : after unlock \n%!" (Utils.domain_name ());
    res

  let unsafe_get t = t.value
  let wait a = Condition.wait a.cond a.mutex
  let signal a = Condition.signal a.cond
  let protect a = Mutex.protect a.mutex
  let lock a = Mutex.lock a.mutex
  let unlock a = Mutex.unlock a.mutex
end
