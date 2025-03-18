(* Possible communication between the two domains: 
+ From Main to Typer :
  - request is canceled
  - merlin is closing
  - is waiting for the lock

+ From Typer to Main :
  - caught an exception 
*)
open Moshared

exception Cancel
exception Closing

module Domain_msg = struct
  type from_main = [ `Empty | `Closing | `Waiting | `Cancel ]
  type from_typer = [ `Empty | `Exn of exn ]

  type msg = {
    from_main : from_main Atomic.t;
    from_typer : from_typer Atomic.t;
  }

  let create () =
    { from_main = Atomic.make `Empty; from_typer = Atomic.make `Empty }

  let send_msg msg new_msg signal_on =
    (* CAS could be replaced by `set` here *)
    if Atomic.compare_and_set msg `Empty new_msg then
      while Atomic.get msg == new_msg do
        Shared.signal signal_on
      done
    else failwith "send_msg: should not happen."
end
