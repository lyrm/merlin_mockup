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

module Wrapper : sig @@ portable
  type t : value mod contended portable = (Vendored.t, k) Capsule.Data.t
  (* type protect = k Capsule.Access.t *)

  val create : unit -> t
  val add_env_p : access:k Capsule.Access.t -> t -> string -> unit
  val concat_all_p : access:k Capsule.Access.t -> t -> string -> unit
  val foo_p : access:k Capsule.Access.t -> int -> bool
end = struct
  type t : value mod contended portable = (Vendored.t, k) Capsule.Data.t
  (* type protect = k Capsule.Access.t *)

  let create_ = Obj.magic_portable @@ Vendored.create
  let create () = Capsule.Data.create create_

  let add_env : Vendored.t -> string -> unit =
    Obj.magic_portable @@ Vendored.add_env

  let add_env_p : access:k Capsule.Access.t -> t -> string -> unit =
   fun ~access env a ->
    let env = Capsule.Data.unwrap ~access env in
    add_env env a

  let concat_all : Vendored.t -> string -> unit =
    Obj.magic_portable @@ Vendored.concat_all

  let concat_all_p : access:k Capsule.Access.t -> t -> string -> unit =
   fun ~access env c ->
    let env = Capsule.Data.unwrap ~access env in
    concat_all env c

  let foo : int -> bool = Obj.magic_portable @@ Vendored.foo
  let foo_p : access:k Capsule.Access.t -> int -> bool = fun ~access c -> foo c
end

let () =
  let counter = Atomic.make 0 in

  let ( let* ) spawn_result f =
    match spawn_result with
    | Multicore.Spawned -> f ()
    | Failed ((), _, _) -> failwith ""
  in

  let mutex = Lock.mutex in

  let w = Wrapper.create () in

  let* () =
    Multicore.spawn
      (fun () ->
        let await = Await_blocking.await Terminator.never in
        Mutex.with_key await mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                Wrapper.concat_all_p ~access w "prout"));
        Atomic.incr counter)
      ()
  in

  let* () =
    Multicore.spawn
      (fun () ->
        let await = Await_blocking.await Terminator.never in
        Mutex.with_key await mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                Wrapper.concat_all_p ~access w "prout"));
        Atomic.incr counter)
      ()
  in

  (* Waiting for all domains to finish *)
  while Atomic.get counter <> 2 do
    Thread.yield ()
  done

(* ================================================================
   Analysis: does this wrapper prevent data races?
   ================================================================

   The Vendored module models two categories of mutable state found in
   merlin's vendored OCaml typer (see MUTABLE_STATE_AUDIT_MERLIN.md):

   1. State reachable through [Vendored.t] (here: [r]).
      Maps to: type_expr mutable fields, trail, environment data — state
      passed through functions like Ctype.unify(env, type_expr).

      Protection: [Vendored.t] is wrapped in [Capsule.Data.t]. The only
      way to unwrap it is via [Capsule.Data.unwrap ~access], which
      requires a [k Capsule.Access.t]. The branding with [k] ties it to
      [Lock.mutex], so all access is serialized. Sound.

   2. Hidden global state not in [Vendored.t] (here: [b]).
      Maps to: iter_env_cont, printing_*, warnings.*, abbreviations,
      simple_abbrevs — plain refs mutated as side effects of vendored
      functions, not reachable through any parameter.

      Protection: indirect. All Wrapper functions require [k Access.t],
      and [k] is abstract, so callers must go through [Lock.mutex] to
      obtain one. This serializes all calls, which happens to protect [b].
      Sound, but the guarantee relies on branding, not on structural
      capsule protection.

   3. Category NOT modeled: Local_store snapshot/restore.
      In merlin, Local_store gives both domains access to the same
      underlying refs but with a snapshot/restore mechanism. The wrapper
      doesn't model this. It matters because the wrapper must ensure the
      Local_store snapshot is consistent for the duration of a critical
      section.

   Bypass analysis (can a user cause a data race using only the Wrapper?):
   - Data.project: safe, returns [contended], can't mutate.
   - Exception smuggling: blocked by the mode system (contended/portable).
   - Key.unsafe_mk: allows forging a key and bypassing the mutex entirely.
     However, it is explicitly named [unsafe] — the convention is that
     well-behaved code doesn't call it.

   Parallelism with a single mutex:
   - A single mutex does NOT mean a single big critical section. The
     wrapper only requires the mutex for each individual wrapper call.
     The caller controls when to acquire and release it:
       * Hold it for a short section (one wrapper call)
       * Hold it longer (multiple calls that need consistency)
       * Release it between independent operations
   - Parallelism comes from the gaps between mutex acquisitions. In
     merlin: the worker holds the mutex while typing, releases it when
     sending the partial result. The main domain acquires it only for
     the vendored calls during analysis (Ctype.unify, Printtyp, Env
     lookups), and releases it for non-vendored analysis steps. Both
     domains run non-vendored code concurrently.
   - The wrapper makes this discipline visible: every vendored call goes
     through the Wrapper and requires [Access.t], so it is clear exactly
     where the mutex is needed.

   Why NOT multiple mutexes (one per category of state)?
   - Vendored functions often touch multiple categories of state in a
     single call (e.g. Ctype.unify touches trail, current_level,
     type_expr fields, abbreviations). Multiple mutexes would require
     acquiring several locks atomically, creating deadlock risks.
   - A single mutex avoids lock ordering issues entirely.

   Limitation of a single mutex:
   - All vendored state is behind one lock, even if two pieces are
     independent (e.g. printing_* and trail). If the worker is unifying
     types and the main domain only needs to print, they block each
     other. In practice this is likely acceptable given how short
     individual vendored calls are.
   - Lock granularity is a runtime discipline, not a compile-time one.
     The compiler ensures you hold the mutex for each wrapper call, but
     not that you hold it long enough for consistency across multiple
     calls (TOCTOU risk). This requires developer discipline.

   Trust boundary:
   - The [Obj.magic_portable] casts are where the unsafety lives: the
     wrapper author must verify that vendored functions are safe to call
     from another domain under the lock. The compiler cannot check this.

   See also:
   - MUTABLE_STATE_AUDIT_MERLIN.md for the full list of mutable state
   - ../../vendored/step1_understanding.md for the wrapper strategy
   - vendored_code_example2.ml for an alternative using With_mutex
   ================================================================ *)
