open! Await
module Lock = Capsule.Mutex.Create ()

type k = Lock.k

module Vendored : sig
  type t

  val create : unit -> t
  val add_env : t -> string -> unit
  val concat_all : t -> string -> unit
  val foo : int -> bool
end = struct
  type r = string list ref
  type t = { r : r }
  type b = int ref

  let r : r = ref []
  let create () = { r }
  let b = ref 0
  let add_env (t : t) a = t.r := a :: !(t.r)

  let concat_all (t : t) (c : string) =
    t.r := List.map (fun s -> s ^ c) !(t.r);
    incr b

  let foo (c : int) =
    b := !b + c;
    !b mod 2 = 0
end

(* ================================================================
   Alternative 1: Use With_mutex to bundle data + mutex together.

   The user gets a single value that can only be accessed through
   a callback that holds the mutex. No Access.t leaks out.
   ================================================================ *)
module Wrapper_with_mutex : sig @@ portable
  type t : value mod contended portable

  val create : unit -> t

  val with_lock :
    Await.t @ local ->
    t ->
    f:(Vendored.t -> 'a @ contended portable) @ local once portable ->
    'a @ contended portable

end = struct
  type t = Vendored.t Capsule.With_mutex.t

  let create_ = Obj.magic_portable @@ Vendored.create
  let create () = Capsule.With_mutex.create create_

  let with_lock await t ~f =
    Capsule.With_mutex.with_lock await t ~f
end

let add_env : Vendored.t -> string -> unit =
  Obj.magic_portable @@ Vendored.add_env

let concat_all : Vendored.t -> string -> unit =
  Obj.magic_portable @@ Vendored.concat_all

let foo : int -> bool = Obj.magic_portable @@ Vendored.foo

let () =
  let counter = Atomic.make 0 in
  let w = Wrapper_with_mutex.create () in
  let await = Await_blocking.await Terminator.never in

  let ( let* ) spawn_result f =
    match spawn_result with
    | Multicore.Spawned -> f ()
    | Failed ((), _, _) -> failwith ""
  in

  let* () =
    Multicore.spawn
      (fun () ->
        Wrapper_with_mutex.with_lock await w ~f:(fun env ->
            add_env env "hello";
            add_env env "world";
            concat_all env "!");
        Atomic.incr counter)
      ()
  in

  let* () =
    Multicore.spawn
      (fun () ->
        Wrapper_with_mutex.with_lock await w ~f:(fun env ->
            add_env env "foo";
            let _ = foo 42 in
            concat_all env "?");
        Atomic.incr counter)
      ()
  in

  while Atomic.get counter <> 2 do
    Thread.yield ()
  done

