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

(* ================================================================
   Analysis: does this wrapper prevent data races?
   ================================================================

   The Vendored module models two categories of mutable state found in
   merlin's vendored OCaml typer (see MUTABLE_STATE_AUDIT_MERLIN.md):

   1. State reachable through [Vendored.env] (here: [entries]).
      Maps to: type_expr mutable fields, trail, environment data — state
      passed through functions like Ctype.unify(env, type_expr).

      Protection: [Vendored.env] is wrapped in [Capsule.Data.t]. The only
      way to unwrap it is via [Capsule.Data.unwrap ~access], which
      requires a [k Capsule.Access.t]. The branding with [k] ties it to
      [Lock.mutex], so all access is serialized. Sound.

   2. Hidden global state not in [Vendored.env] (here: [global_counter]).
      Maps to: iter_env_cont, printing_*, warnings.*, abbreviations,
      simple_abbrevs — plain refs mutated as side effects of vendored
      functions, not reachable through any parameter.

      Protection: indirect. All Wrapper functions require [k Access.t],
      and [k] is abstract, so callers must go through [Lock.mutex] to
      obtain one. This serializes all calls, which happens to protect
      [global_counter]. Sound, but the guarantee relies on branding, not
      on structural capsule protection.

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
