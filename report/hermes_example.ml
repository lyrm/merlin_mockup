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

(* ******************* *)

(* type state = { mutable count : int } *)

(* let process (s : state) par =
  let read _par = s.count in
  let incr _par = s.count <- s.count + 1 in

  (* [sum] compiles in fork_join2 but not [inc].

     Hovering over [sum] would show:
       ('a -> int) @ shareable
     Hovering over [inc] would show:
       ('a -> unit) @ nonportable

    making it clear [sum] is compatible with fork_join2 while [inc] is not.
  *)
  let #(_, ()) = Parallel_kernel.fork_join2 par read incr in
  () *)

(* ******************* *)

(* let foo () =
  let r = ref None in

  let bar () = !r in

  let _ = Multicore.spawn bar () in

  r := Some 43 *)

(* ******************* *)

(* module T : sig
  val foo_p : unit -> int option @@ portable
  val foo_np : unit -> unit
end = struct
  let a = ref (Some 42)
  let foo_np () = a := Option.map (( + ) 1) !a
  let foo_p () = Some 12
end

let foo par = Multicore.spawn T.f () |> ignore *)

(* ******************* *)

(* open Core

let f () =
  let l = stack_ [ 1; 2; 3 ] in
  List.iter__local l ~f:(fun _ -> ()) [@nontail]
(*   ^^
     Completion shows [iter], [map], [filter], ...
     but with a local list, the global variants won't work.
     Mode-aware completion would indicate which variants
     accept a local argument. *) *)

(* ******************* *)

(* let apply t (foo : _ -> unit) =
  Mutex.with_access (Await_blocking.await Terminator.never) Lock.mutex
    ~f:(fun access -> foo access) *)

(* ******************* *)

(* let foo () =
  let free (_ @ unique) = () in
  let f _ = () in

  let r = Some 42 in

  let _ = f (r : int option @ many) in
  let _ = f (r : int option @ many) in
  let _ = free (r : int option @ unique) in
  () *)

(* ******************* *)

(* let foo await mutex data =
  Mutex.with_access await mutex ~f:(fun access ->
      (* this closure: @ portable once local *)
      (*   because: Mutex.with_access requires ~f @ portable *)
      let r = Capsule.Data.unwrap ~access data in
      (* r : int ref @ aliased uncontended *)
      (*   because: unwrap inside with_access gives uncontended *)
      incr r;
      !r) *)
(* return value: int @ contended portable *)
(*   because: with_access returns @ contended portable *)

(* ******************* *)

(* let foo () =
  let r @ uncontended = ref (Some 42) in

  let _ = Multicore.spawn (fun () -> r := Option.map (( + ) 1) !r) () in

  (r : int option ref @ uncontended) := Some 43 *)

(* ******************* *)

(* open! Await
module Lock = Capsule.Mutex.Create ()

let data : (int list ref, Lock.k) Capsule.Data.t =
  Capsule.Data.create (fun () -> ref [])

let make_processor () =
  let cache = ref [] in
  let f x =
    cache := x :: !cache;
    List.length !cache
  in
  f

let example () =
  let f = make_processor () in
  (* ... 50 lines of code ... *)
  let await = Await_blocking.await Terminator.never in
  Mutex.with_access await Lock.mutex ~f:(fun access ->
      let data = Capsule.Data.unwrap ~access data in
      let _ = f !data in
      ()) *)

(* ******************* *)

(* let consume (x @ unique) = ignore x

let example (x @ unique) =
  let f () = consume x in
  let g () = f () in
  g ();
  g () *)
(* error: g is once, already used *)

(* ******************* *)
(*
let make_config () = exclave_ stack_ ("host", 8080)


let wrap_config () =
  let cfg = make_config () in
  (* cfg is local*)
  let wrapped = (cfg, "metadata") in
  (* wrapped is local *)
  wrapped 
  (* error: local, expected global *)
*)
