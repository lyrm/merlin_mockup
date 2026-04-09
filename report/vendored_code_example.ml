open! Await
module Lock = Capsule.Mutex.Create ()

type k = Lock.k

(* ================================================================
   Vendored module: simulates code we can't change (e.g. the OCaml
   typer vendored in merlin). Contains two kinds of mutable state:

   - [env]: a mutable record field, visible in the signature.
     Analogous to type_expr mutable fields or environment data.

   - [global_counter]: a hidden module-level ref, not in the signature.
     Analogous to current_level, trail, iter_env_cont, etc.

   Both are mutated as side effects of the public functions.
   ================================================================ *)
module Vendored : sig
  type env

  val create : unit -> env
  val add_entry : env -> string -> unit
  val compute : int -> bool
end = struct
  type env = { entries : string list ref }

  (* Hidden global states *)
  let global_counter = ref 0
  let entries = ref []
  let create () = { entries }

  let add_entry env s =
    env.entries := s :: !(env.entries);
    incr global_counter

  let compute n =
    global_counter := !global_counter + n;
    !global_counter mod 2 = 0
end

(* ================================================================
  Naive wrapper: re-exports vendored functions as portable without any protection.
   ================================================================ *)
module Wrapper_naive : sig @@ portable
  type env = Vendored.env

  val create : unit -> env
  val add_entry : env -> string -> unit
  val compute : int -> bool
end = struct
  type env = Vendored.env

  let create = Obj.magic_portable @@ Vendored.create
  let add_entry = Obj.magic_portable @@ Vendored.add_entry
  let compute = Obj.magic_portable @@ Vendored.compute
end

(* ================================================================
   Wrapper: re-exports vendored functions as portable, and requires
   a [k Capsule.Access.t] to call them. This ensures the caller
   holds the mutex before touching any vendored mutable state.

   The wrapper is the trust boundary: internally it uses
   [Obj.magic_portable] to cast vendored functions. The correctness
   is verified by the wrapper author, not by the compiler.
   ================================================================ *)
module Wrapper : sig @@ portable
  type env : value mod contended portable = (Vendored.env, k) Capsule.Data.t

  val create : unit -> env
  val add_entry : access:k Capsule.Access.t @ local -> env -> string -> unit
  val compute : access:k Capsule.Access.t @ local -> int -> bool
end = struct
  type env : value mod contended portable = (Vendored.env, k) Capsule.Data.t

  let create_ = Obj.magic_portable @@ Vendored.create
  let create () : env = Capsule.Data.create create_
  let add_entry_ = Obj.magic_portable @@ Vendored.add_entry

  let add_entry ~(access : k Capsule.Access.t @ local) env s =
    add_entry_ (Capsule.Data.unwrap ~access env) s

  let compute_ = Obj.magic_portable @@ Vendored.compute
  let compute ~(access : k Capsule.Access.t @ local) n = compute_ n
end

(* ================================================================
   Usage: two domains calling wrapped vendored functions.
   Each domain acquires the mutex before calling any wrapper function.
   ================================================================ *)
let () =
  let counter = Atomic.make 0 in
  let mutex = Lock.mutex in
  let env = Wrapper.create () in

  let ( let* ) spawn_result f =
    match spawn_result with
    | Multicore.Spawned -> f ()
    | Failed ((), _, _) -> failwith "spawn failed"
  in

  (* Domain 1: simulates the typer domain *)
  let* () =
    Multicore.spawn
      (fun () ->
        let await = Await_blocking.await Terminator.never in
        Mutex.with_key await mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                Wrapper.add_entry ~access env "hello";
                Wrapper.add_entry ~access env "world"));
        Atomic.incr counter)
      ()
  in

  (* Domain 2: simulates the analysis domain *)
  let* () =
    Multicore.spawn
      (fun () ->
        let await = Await_blocking.await Terminator.never in
        Mutex.with_key await mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                Wrapper.add_entry ~access env "foo";
                let _ = Wrapper.compute ~access 42 in
                ()));
        Atomic.incr counter)
      ()
  in

  while Atomic.get counter <> 2 do
    Thread.yield ()
  done
