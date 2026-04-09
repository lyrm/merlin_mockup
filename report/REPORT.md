# Experience report of portabilizing `merlin-domains` to OxCaml

*Portabilizers:* Carine Morel, Timéo Arnouts

*Author:* Carine Morel

*Reviewers:* Sonja Heinze, Timéo Arnouts, Ulysse Gérard, Xavier Van de Woestyne

*Supervisor:* Sonja Heinze

This report documents our experience portabilizing a multicore OCaml project to OxCaml. We describe the challenges we encountered, the solutions we found, and suggestions for improving the OxCaml developer experience.

<!-- TODO : Add a note about the fact that we only took advantage of the DRF axes, not the other ones. The mockup is btw only useful for these axes. -->

<!-- TODO : oxcaml multicore programming requires a lot of deps (parallel, await etc..) -->

## Introduction

<!-- This part is meant to be kept short, interesting details being developed in further sections. The objective is to give the context and how we encounter the main challenges we face.


- DONE Quick overview
- ~~TODO (?) Message passing data structure : not in this part~~
- DONE What did we actually portabilize: the mockup
- TODO: add a short paragraph explaining that we portabilized the mock-up rather than the real project, and why. -->

### Background of the contributors

Two people worked on the portabilization: myself (Carine) and Timéo Arnouts. Neither of us had prior practical experience with OxCaml or Rust.

- *Carine* — Senior engineer. Wrote the multicore part of `merlin-domains` as well as the original mock-up. Had read the three original blog posts about OxCaml roughly a year before the project and had some familiarity with the concepts from internal discussions, but no hands-on experience.

- *Timéo* — Junior engineer. No prior knowledge of OxCaml.

### About `merlin-domains`

We begin with a brief overview of the project and the context of our work: `merlin-domains` (TODO: add link to repo) is an experimental branch of merlin that aims to leverage multicore programming to improve Merlin's performance. The general idea is to run in parallel a secondary domain to do most of the computation (typing) while the main domain mostly does query dispatching and execution of the query logic. This enables three performance-oriented features:

- Early type return: if permitted by the request, the secondary domain shares a partial typedtree with the main domain as soon as possible, so the main domain can perform the final analysis on it and answer the request without waiting for the full buffer to be typed. Meanwhile, the secondary domain goes back to finish typing the rest of the buffer. This optimization is especially useful for large buffers modified near the beginning.

- Parallelization: the two performance bottlenecks in Merlin are the typing and the final analysis. By running them in parallel thanks to early type return, we can reduce the overall latency of a request. This proves to be difficult as both steps mutate the shared state, creating data races. Some work is still needed to analyse the data races, and find finer-grain solution to avoid them and make the parallelization work better.

- Cancellation mechanism: once the early type return is achieved, the main domain is back to listening for new requests while the secondary domain may still be typing the end of the buffer. If a new request arrives, the main domain can send a cancellation message to the secondary domain to stop the current work and start processing the new request.
  
TODO : add sequence diagram from Lambda World

### Reasons for portabilizing `merlin-domains` to OxCaml

`merlin-domains` is a real-world multicore OCaml project, making it a compelling candidate for portabilization to OxCaml:

- **Right level of complexity:** `merlin` is a large, real-world project, far from a toy example: portabilizing it is genuinely challenging. However, its multicore design is relatively simple (only two domains, no scheduler), which makes it a tractable first target. It also features shared mutable state and data races, which OxCaml statically prevents: this means the portabilization must address them explicitly, exercising a key part of OxCaml's safety model.

- **Availability of a mock-up:** to experiment with different multicore designs, we wrote a mock-up of the project (TODO: add link). Since we were not very familiar with OxCaml when we started, the mock-up also served as a fallback in case we could not portabilize the real project, which ended up being the case (TODO: link to a more complete explanation ?). The mock-up was progressively complexified to incorporate patterns from the real project that we identified as potential blockers for portabilization.

- **Concrete challenges representative of other projects**: when portabilizing a project to OxCaml, some code may need to remain in plain OCaml: whether it comes from external libraries, vendored dependencies, or parts of the codebase that are not worth portabilizing. This OCaml code still needs to be interfaced with OxCaml. In `merlin-domains`, vendored code from OCaml typer that contains mutable values is a concrete instance of this challenge (TODO: link to below).

- **Message-passing data structure:** `merlin-domains` relies on a relatively simple message-passing data structure, which makes it an interesting starting point for exploring how such patterns translate to OxCaml (TODO: link to below).

<!-- TODO: did I miss any other reason -->

<!-- TODO :  Past me: should we do a section about the different features we add to the mockup to simulate poossible blockers for the portabilization ? Or just a line about the fact that we complexified the mockup to introduce some of the challenges we identified in the real project ? -->
<!-- Current me : but we do that with the challenges below, right ? -->

## The successes
### Learning OxCaml
- hard but the documentation help a lot (todo: link to the improvement part below)
- possible even with API changes 

### Portabilizing the mock-up
- we portabilize merlin-domain mockup 

- make us more aware of the data races: we had to deal with all data races and find solution to ensure DRF even with the vendored code


### Make us think of a better design for merlin-domains
OxCaml: did it help with the general design ? 
- fork/join : better design -> be more aware of what is shared (read/write) and use the right level of synchronization 


## The challenges

Portabilizing `merlin-domains` to OxCaml was a challenging experience, especially since it also includes learning OxCaml from scratch. Below, we describe the main challenges we encountered. As noted before, we focused our effort on leveraring OxCaml DRF guarantee to make the parallelization work, and we did not take advantage of the other features of OxCaml (e.g. locality axis and unboxed types), which is why we don't mention challenges specifically related to these features below.

When evaluating the feasability of the project, we identified the following expected challenges:
- learning OxCaml,
- which concurrency model to use,  
- how to guarantee DRF without changing the code vendored from OCaml typer ? 

We also encountered some non-expected challenges: 
- the interdependency between the different features of OxCaml (modes, modalities, kinds, unboxed types) to do multicore programming in OxCaml,
- errors messages: understanding them but also understanding the interconnexion between type error and mode error. 

The two main challenges ended up being learning OxCaml and dealing with the vendored code. They are the core reasons we chose to portabilize the mockup instead of merlin-domains itself: we needed a simpler codebase to learn and explore possible approaches on how to deal with vendored code.  

### Learning OxCaml

#### Context
 We had no prior practical experience with OxCaml, and we had to learn it from scratch while portabilizing `merlin-domains`. This was a significant challenge, as OxCaml has a steep learning curve. 

#### Challenge
We encountered two main challenges as newcomers to OxCaml trying to portabilize multicore code: understanding modes, and navigating the capsule API.

The first challenge is related to the diversity of modes. Each mode axis has its own logic, and there are many of them. This makes it difficult to develop a good intuition for when and how to use them. In practice, learning OxCaml translated for us in many trial and error, or going back and forth between the code and the [documentation](https://oxcaml.org/documentation/modes/intro/) to understand why a given mode is inferred and how to satisfy its constraint. It took us quite some time to even start portabilizing the code.

The second challenge was navigating the capsule API, which was the more significant challenge of the two, for several reasons:

- *Size.* The API is very long, making it hard to know which part to focus on when starting out.

- *Fragmentation.* The API is exposed through many libraries: `capsule0`, `capsule`, `await`, `portable`, `core`. Each provides a different subset or wrapping of the same underlying types. Although they are all mostly* compatible, they do not expose the same functions, which makes it hard to know where to look for what one needs, especially combined with the size of these APIs.

**Mostly compatible, because there are some incompatibilities between the different libraries. This, for example, does not compile.*

```ocaml
open Await

let read () =
  (* Create a capsule and a mutex using the blocking_sync API
   from the [capsule] library *)
  let (Capsule.Expert.Key.P key) = Capsule.Expert.create () in
  let blocking_mutex = Capsule.Blocking_sync.Mutex.create key in
  let data : (int ref, _) Capsule.Data.t =
    Capsule.Data.create (fun () -> ref 0)
  in
  (* Try to use it with Await.Mutex.with_access. It won't compile
   because Capsule.Blocking_sync.Mutex.t is not Await.Mutex.t *)
  let await = Await_blocking.await Terminator.never in
  Mutex.with_access await blocking_mutex ~f:(fun access ->
      !(Capsule.Data.unwrap ~access data))
```

- *Mode prerequisites.* Using capsules effectively requires understanding almost all mode axes: portability and contention for DRF, but also contention for access, locality for password, linearity and uniqueness for key usage. Kinds and modalities are also needed to understand mode crossing and make everything work. Learning just the portability and contention axes is not enough.

- *Lack of guided material.* The API is well documented in its `.mli` files, but reference documentation alone is not enough to build an understanding for when to use each way of opening a capsule (access, password, key). 
<!-- More guided material, like tutorials or annotated examples showing why one approach is needed over another, would have made a real difference.  -->

- *Verbosity.* Using the API requires a lot of boilerplate code, which tends to hide the core logic and make it harder to understand what is going on, especially for concurrent algorithms. Here is an example of how the API can make simple logic look more complex. This code is mostly extracted from the message-passing data structure we implemented for `merlin-domains`:

In OCaml: 
```ocaml
type 'a t = 
{ mutex : Mutex.t; 
  cond : Condition.t; 
  mutable msg : msg }

let send_and_wait t new_msg =
  Mutex.protect t.mutex (fun () ->
    t.msg <- new_msg;
    Condition.signal t.cond;
    while t.msg == new_msg do
      Condition.wait t.cond t.mutex
    done) 
```

In OxCaml:
```ocaml
module Lock = Capsule.Mutex.Create ()
type k = Lock.k

type 'a t : value mod contended portable = {
  mutex : k Mutex.t;
  cond : k Mutex.Condition.t;
  msg : (msg ref, k) Capsule.Data.t;
}

let send_and_wait t new_msg =
  let await = Await_blocking.await Terminator.never in
  Mutex.with_key await t.mutex ~f:(fun key ->
      let #((), key) =
        Capsule.Expert.Key.access key ~f:(fun access ->
            let msg = Capsule.Data.unwrap ~access t.msg in
            msg := new_msg)
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
```

The core logic is the same in both versions (set the message, signal, wait until cleared), but in OxCaml it is interleaved with capsule unwrapping, key threading, and mode annotations, which makes it harder to follow at a glance.

#### Approach
To learn OxCaml, the [oxcaml.org tutorials](https://oxcaml.org/documentation/tutorials/01-intro-to-parallelism-part-1/) and discussions with the JS team (special thanks to Liam and Aspen) were essential. For the capsule API specifically, we relied mostly on trial and error as no dedicated tutorial exists.

#### Take-away
The learning curve could be made less steep with more guided material for the capsule API and better editor support for modes. We discuss concrete suggestions in the [editor support](#helping-the-user-through-editor-support) and [documentation](#making-the-learning-curve-less-steep) sections below.


### Finding the right concurrency primitive

#### Context
`merlin-domains` uses a two-domain architecture with a message-passing coordination mechanism: the main domain handles request dispatching while a dedicated secondary domain performs the bulk of the computation (typing and the rest of the merlin pipeline). The two domains communicate through a shared synchronization structure using a blocking send/acknowledge protocol.

Here is a simplified version of the main function, in OCaml, that illustrates the interaction between the main domain and the secondary domain (typer) in `merlin-domains`:
```ocaml
let main () =
  (* Hermes is a data structure used to share data in between domains *)
  let hermes = Hermes.create (fun () -> None) in

  let typer =
    Domain.spawn (fun () ->
        let rec loop () =
          match Hermes.recv_clear hermes with
          | Config config ->
              (* some computation *)
              process config hermes;
              loop ()
          | Msg `Closing -> ()
        in
        loop ())
  in

  (try
     Server.listen ~handle:(fun req ->
         match req with
         | Server.Close -> raise Closing
         | Server.Config config ->
             (* Send work to the typer domain *)
             Hermes.send_and_wait hermes (Config config);
             (* Does more stuff and answer the request *)
             let r = run config hermes in
             answer r)
   with _ -> ());

  Hermes.send_and_wait hermes (Msg `Closing);
  Domain.join typer
```

#### Challenge
We initially planned to use `Parallel.fork_join2` and rework our design to fit its fork/join model. However, `Parallel` provides *parallelism*, not *concurrency*: it is always a valid schedule to run one task to completion before starting the other. Our message-passing data structure requires *concurrency*: one task blocks on a condition variable until the other signals it. With `Parallel`, this leads to a deadlock.

Extract of the message-passing data structure written in OCaml:
```ocaml
type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable msg : 'a option }

let send_and_wait t msg =
  Mutex.protect t.mutex (fun () ->
    let new_v = Some msg in
    t.msg <- new_v;
    Condition.signal t.cond;
    while t.msg == new_v do
      Condition.wait t.cond t.mutex
    done)
```
`send_and_wait` blocks until the other task calls `Condition.signal`. If the scheduler runs the first task to completion before starting the second, the signal never arrives.

`Concurrent` (which guarantees that runnable tasks make progress within finite time) would fit our needs, but at the time it did not provide additional value over `Multicore` for our use case.

#### Approach
The best match we found for our use case is the `Multicore` library, which spawns actual domains and thus provides the concurrency guarantee we need: both tasks run independently and can make progress regardless of whether the other is blocked. The resulting code is very similar to the original `Domain.spawn` version in OCaml, except it provides the right modes to ensure DRF.

#### Take-away

- There is very little documentation about which scheduling library to use for what. Only `Parallel` is covered in the oxcaml.org documentation; `Concurrent` and `Multicore` are not mentioned. Guidance on when to use each would help users avoid the kind of deadlock we encountered.

- Using a fork-join design would have helped us refine: (1) when concurrency (rather than parallelism) is needed, and (2) what must be shared between the two tasks at each fork.  <!-- TODO : LINK -->

### Dealing with top level mutable state 

`merlin` uses a lot of top-level mutable state, both in its own code and in the vendored OCaml typer. In OxCaml, top-level mutable values can't be shared between domains. For merlin's own code, which we can modify, the fix is straightforward: wrap the mutable state in `Capsule.Data.t` (or in an `Atomic.t` if the operation performed on the value are translatable in atomic operations).

```ocaml
(* Before: nonportable, can't be shared *)
let res = ref []

(* After: portable, accessible under the mutex *)
let res : (typedtree, 'k) Capsule.Data.t =
  Capsule.Data.create (fun () -> ref [])
```

For the vendored code, which we should not modify, the problem is more complex and is addressed in the next section.

### Integrating vendored code

#### Context

Dealing with vendored code was one of the main reasons we portabilized the mock-up rather than the real project: we needed a simpler codebase to explore possible approaches.

`merlin` vendors a large portion of the OCaml compiler. It is written  in plain OCaml, was designed for a single-threaded world, and is full of mutable state: roughly 40+ module-level refs and mutable record fields. This vendored code should not be deeply rewritten in OxCaml: it is rebased onto each new OCaml release, and any modification would have to be redone at every rebase. Here we choose the extreme option of not altering the code at all. A similar situation would arise with any external library that is not portabilized to OxCaml: we want to use it as is, without modifying it, but we still need to interface with it from OxCaml code.

In `merlin-domains`, both domains call into vendored code: the worker domain runs the typer, and the main domain runs analysis code (which also calls vendored functions like `Ctype.unify` or `Printtyp.wrap_printing_env`). During the partial result scenario, both execute concurrently, creating data races on the vendored mutable state.

#### Challenge

The vendored code is plain OCaml: its functions are `nonportable` and can't be called from a `portable` context (e.g. inside a `fork_join` or a capsule callback). The most straightforward approach is to wrap them with `Obj.magic_portable`:

```ocaml
module Vendored : sig
  type env
  val create : unit -> env
  val add_entry : env -> string -> unit
  val compute : int -> bool
end = struct
  type env = { entries : string list ref }
  let global_counter = ref 0

  let add_entry env s =
    env.entries := s :: !(env.entries);
    incr global_counter
  
  let compute n =
    global_counter := !global_counter + n;
    !global_counter mod 2 = 0
  ...
end

(* Naive wrapper: makes vendored functions portable, but provides
   no protection against data races. *)
module Wrapper_naive : sig @@ portable
  val add_entry : Vendored.env -> string -> unit
  val compute : int -> bool
end = struct
  let add_entry = Obj.magic_portable @@ Vendored.add_entry
  let compute = Obj.magic_portable @@ Vendored.compute
end
```

This makes the functions callable from both domains, but nothing prevents two domains from calling them concurrently. Both `add_entry` and `compute` mutate hidden global state so calling any combination of these from two domains in parallel is a data race. The naive wrapper compiles, but it is no safer than plain OCaml.

We needed to find a way to leverage OxCaml's mode system to enforce mutual exclusion on vendored function calls at compile time, without being able to change the vendored code itself.

#### Approach

Our approach is to require a branded `Capsule.Access.t` to call any wrapper function. We create a single mutex and tie the wrapper to its brand `k`. We also wrap the visible mutable state (`Vendored.env`) in a `Capsule.Data.t`, so that unwrapping it requires the same `Access.t`:

```ocaml
module Lock = Capsule.Mutex.Create ()
type k = Lock.k

module Wrapper : sig @@ portable
  type env : value mod contended portable = (Vendored.env, k) Capsule.Data.t

  val create : unit -> env
  val add_entry : access:k Capsule.Access.t -> env -> string -> unit
  val compute : access:k Capsule.Access.t -> int -> bool
end = struct
  type env : value mod contended portable = (Vendored.env, k) Capsule.Data.t

  let create_ = Obj.magic_portable @@ Vendored.create
  let create () : env = Capsule.Data.create create_
  let add_entry_ = Obj.magic_portable @@ Vendored.add_entry

  let add_entry ~(access : k Capsule.Access.t) env s =
    add_entry_ (Capsule.Data.unwrap ~access env) s

  let compute_ = Obj.magic_portable @@ Vendored.compute
  let compute ~(access : k Capsule.Access.t) n = compute_ n
end
```

The key idea: the only way to obtain a `k Capsule.Access.t` is through `Lock.mutex`, so callers are forced to hold the mutex before calling any wrapper function. Since `k` is abstract, no other mutex can produce a compatible `Access.t`. This gives a compile-time guarantee that all vendored code runs under mutual exclusion.

For `add_entry`, the `Access.t` serves a double purpose: it is needed both to unwrap `env` from its `Capsule.Data.t` and to authorize the call (which mutates hidden global state). Also wrapping `env` in a capsule is not strictly necessary for the mutex discipline, it makes it explicit in the signature that `env` is stateful.

For `compute`, which does not take an `env` argument, the `Access.t` only serves as an authorization token: it is not structurally needed, but requiring it ensures the caller holds the mutex. In both cases, the wrapper is the trust boundary: `Obj.magic_portable` casts are used inside, and the wrapper author must verify that the vendored functions are safe to call under the lock. Outside the wrapper, the compiler enforces the discipline: no `Access.t`, no call.

On the caller side, this looks like:

```ocaml
let await = Await_blocking.await Terminator.never in
Mutex.with_key await Lock.mutex ~f:(fun key ->
    Capsule.Expert.Key.access key ~f:(fun access ->
        Wrapper.add_entry ~access env "hello";
        let _ = Wrapper.compute ~access 42 in
        ()))
```

We use a single mutex for all vendored state. This does not mean a single big critical section: the caller controls when to acquire and release the lock, and concurrency comes from the gaps between acquisitions. We chose a single mutex because many vendored functions touch multiple state at once, making fine-grained locking impractical and deadlock-prone.

#### Take-away

- *The branded `Access.t` pattern is a general approach* for interfacing OxCaml code with non-OxCaml dependencies: the compiler enforces the mutex discipline at the boundary, without modifying the dependency.

- *The guarantee is only as good as the wrapper*: a vendored function missing from the wrapper can be called without the mutex. The wrapper must be audited manually.

- *The wrapper maintenance cost seems acceptable for merlin*: it only needs updating when the vendored API surface changes (new or modified function signatures), not for internal refactors.

- *Open question*: is wrapping visible state in `Capsule.Data.t` worth the added verbosity? The `Access.t` token already forces the mutex, so the structural protection it adds seems marginal.

- *OxCaml to OCaml side note*: As a side note, we wondered whether it would be possible to write the concurrent parts of a project in OxCaml for the DRF guarantees, then extract plain OCaml from it. This would probably require some form of code extraction, but could be useful for projects that can't fully switch to OxCaml.

### Interaction between type errors and mode errors

We document here two non-obvious behaviors with mode errors that we encountered. We propose no solution; these are observations that may be worth investigating. Both come from the fact that mode crossing depends on the type of a value, and the type may not be fully resolved when the mode is checked.

#### Mode errors fixed with a type annotation

```ocaml
type msg = Empty | Msg of int option

(* Without annotation: the mode of x is checked at line 6 before
   the match on line 8 constrains x to [msg]. The compiler doesn't
   know x is an immutable variant that crosses contention. *)
let foo par x =
  let #(v, ()) =
    Parallel_kernel.fork_join2 par (fun _par -> x) (fun _par -> ())
    (*                                          ^       *)
    (*  Error: x is "shared" but expected "uncontended" *)
  in
  match v with Empty -> Empty | Msg opt -> Msg (Option.map (( + ) 1) opt)

(* With annotation: [msg] is known at the point where modes are checked.
   [msg] is immutable data, so it crosses contention. It compiles. *)
let foo' par (x : msg) =
  let #(v, ()) =
    Parallel_kernel.fork_join2 par (fun _par -> x) (fun _par -> ())
  in
  match v with Empty -> Empty | Msg opt -> Msg (Option.map (( + ) 1) opt)
```

The mode error disappears with a type annotation. This is confusing because a mode error naturally leads one to look for a mode fix rather than a type fix.

#### Mode error that masks a type error

The following is a simple example where fixing a type error also fixes a mode error. The same pattern exists in plain OCaml (one type error can mask another), but mode errors push developers toward mode fixes, which may not be the root cause. 

```ocaml
let make () = exclave_ stack_ ("hello", 42)

(* Mode error: p is local, can't be returned. *)
let f () =
  let p = make () in
  p

(* Type error: f () returns (string * int), not int.
   But this error is hidden because f doesn't compile. *)
let g () =
  let x : int = f () in
  x + 1
```

The compiler reports the mode error on `p` in `f` and never shows the type error in `g`. Returning `snd p` instead of `p` fixes both: `int` crosses locality, and the type matches `g`'s expectation.

`merlin` already provides a good partial answer to this problem by showing the type error in `g` as a secondary error, but it is still easy to miss the dependency between both errors (obviously not in this oversimplified example).



<!-- ## Detailed description of the technical challenges we faced and how we solved them (if we did)
This session should provide concrete examples and a comprehensive explanation of the challenges, how we encountered them and how we solved them or not and why. 

- Learning OxCaml -> mostly solve through discussions and documentation
  - link to the related discuss post 
  - tail call and locality 

- Capsule API: from 10 lines to 100 lines (the message passing data structure)
  - verbosity 
  - complexity of the API (password, access, key etc..), and compatibility issue between different part of the API (TODO : check is this is still the case)

- The error message about mode can appeared before type it the scope of the mode error is contained in the scope of the type error. It seems like most of the time it is a bad idea to correct the mode error before the type error.

- Vendored code: can't be change. How to do the interface ? -->


## Suggestions for improving the developer experience

Based on our experience, we suggest two directions for making OxCaml more accessible:
- *Editor support*: features to help developers inspect modes, navigate mode-expanded APIs, and understand mode inference through their own code.
- *Guided learning*: a pedagogical tool to help developers build intuition about modes incrementally, through exercises and repetition.

### Helping the user through editor support

The same way `rust-analyzer` provides indicators and hints to help deal with lifetime and ownership, we think that editor support could be extremely helpful with OxCaml. It could help developers understand what has been inferred, navigate APIs made larger by mode variants, and develop the intuition needed to work with modes.

Based on our experience portabilizing `merlin-domains`, we suggest four directions for editor features that we think would be especially helpful:
- *Inspecting modes*: seeing what modes a value has, and which ones matter for its type.
- *Mode-aware completion*: filtering completion and search results by mode compatibility, to help navigate APIs that expose many variants of the same function for different modes.
- *Error message readability*: highlighting all code locations referenced in a mode error, not just the error site.
- *Understanding mode inference*: tracing why a given mode was inferred, to help diagnose errors and develop intuition.

These are suggestions: we have not studied their feasibility. The first three seem reasonable to implement; the last one would definitely require more research.

#### Inspecting modes

From what we know, this is a feature that may have already been implemented internally at Jane Street but has not been released yet. In case it hasn't, here is a short description of what it could look like and why it would be useful.

Like type inspection, mode inspection is a straightforward but essential feature. For example:

```ocaml
type state = { mutable count : int }

let process (s : state) par =
  let read _par = s.count in
  let incr _par = s.count <- s.count + 1 in

  (* [read] compiles in fork_join2 but not [incr].

     Hovering over [read] would show:
       ('a -> int) @ shareable
     Hovering over [incr] would show:
       ('a -> unit) (all modes are default)
     A specific command could show all the modes:
       ('a -> unit) @ global aliased many nonportable uncontended

    making it clear [read] is compatible with fork_join2 while [incr] is not.
  *)
  let #(_, ()) = Parallel_kernel.fork_join2 par read incr in
  () 
```
 
Concretely:
- Modes should be available via hovering or a command that adds annotations, similar to type-enclosing increase-verbosity feature.
- A growing/shrinking selection would help manage the information: show only non-default modes first, then all pertinent modes (i.e. the modes NOT crossed by its kind), then all modes.


#### Mode-aware completion

Inside a mode-constrained context, many functions are not usable, whether they come from local definitions or external libraries. Today, completion shows them all, and the developer discovers the incompatibility only after selecting one.

With local definitions:
```ocaml
module T : sig
  val foo_p : unit -> int option @@ portable
  val foo_np : unit -> unit
end = struct
  let a = ref (Some 42)
  let foo_np () = a := Option.map (( + ) 1) !a
  let foo_p () = Some 12
end

let foo par = Multicore.spawn T.__ () |> ignore
(*                              ^^
   Completion shows both [foo_p] and [foo_np], but only
   [foo_p] is compatible with [spawn]’s portable requirement. *)
```

With external libraries:
```ocaml
open! Core

let foo () =
  let l = stack_ [ 1; 2; 3 ] in
  List.__ l ~f:(fun _ -> ()) [@nontail]
  (*   ^^
     Completion shows [iter], [map], [filter], ...
     but with a local list, the global variants won't work. *)
```

Mode-aware completion would work the same way type-aware completion already does: by ranking results based on compatibility with the current context. In the `List` example, `List.iter__local` should be ranked higher than `List.iter` when the argument is local. In the `Multicore.spawn` example, `foo_p` should rank higher than `foo_np`. It could also provide a short hint about incompatible candidates (e.g. “`List.iter` requires a global argument, but `l` is local”).

#### Using the editor to improve error message readability

Mode error messages often reference several locations in the code (the error site, the capture site, the constraint source, etc.). Today, the editor only underlines the error site. A simple improvement would be to expose all referenced locations through LSP diagnostics and highlight them in the editor. For example, the following code:

```ocaml
let state = ref 0

let f par =
  let g x = state := x in
  let h () = g 42 in
  Parallel_kernel.fork_join2 par
    (fun _par -> h ())
    (fun _par -> ())
```

produces this error:
```
The value "h" is "nonportable"
  because it closes over the value "g" at line 4, characters 13-14
    which is "nonportable"
    because it contains a usage (of the value "state" at line 4, characters 12-17)
      which is expected to be "uncontended".
  However, the value "h" highlighted is expected to be "shareable"
    because it is used inside the function at line 7, characters 4-22
```

The error message already contains all four locations (`state`, `g`, `h`, the closure). Highlighting them in the editor would make the chain immediately visible without having to read the full text.

This feature would also be useful for plain OCaml type errors, which similarly reference multiple locations. It could also be a good first step towards the more ambitious "understanding mode inference" feature described below.

#### Understanding mode inference

When a mode error occurs, the compiler reports the conflict at the use site and while the error messages are usually good, they do not always trace back to the root cause. This can be confusing when the value’s mode was determined far from where it is used. For example:

```ocaml
open! Await
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
  let foo = make_processor () in
  (* ... 50 lines of code ... *)
  let await = Await_blocking.await Terminator.never in
  Mutex.with_access await Lock.mutex ~f:(fun access ->
      let data = Capsule.Data.unwrap ~access data in
      let _ = foo !data in
      ())
```

The compiler rejects this with `”foo” is “nonportable” but expected to be “portable”` on `foo` at line 21. But it does not explain *why* `foo` is nonportable. The root cause is that `make_processor` captures a mutable `cache` ref (line 9), which makes the returned closure nonportable. If `make_processor` is defined in a different module or far away, the developer has no easy way to find this.

An editor feature that, on command, displays the full inference chain for a selected value would help. For example, selecting `foo` at line 21 could show:

```
foo : int list -> int
  line 16: bound @ nonportable
    because: returned from [make_processor]
    because: captures [cache] (mutable ref, line 9)
  line 21: required @ portable
    because: used inside Mutex.with_access ~f (which requires @ portable)
```

This is similar to what the compiler already computes for some errors, but exposed on demand for any value, including in code that compiles successfully. It could be displayed in a side panel or using code-lenses.

Here is another example, on the linearity axis: 
```ocaml
let consume (x @ unique) = ignore x

let example (x @ unique) =
  let f () = consume x in
  let g () = f () in
  g ();
  g ()
```

The trace could show: 
```
g : unit -> unit
  line 5: bound @ once
    because: captures [f] (line 4) which is @ once
    because: [f] captures [x] which is @ unique
  line 7: error — once, already used at line 6
```
Whereas the compiler only reports that `g` is `once` and was already used at line 6, without explaining the `g` → `f` → `x @ unique` chain.

Such traces could be added to the error message itself, but they are already pretty long. An "on-demand" feature seems more appropriate to avoid overwhelming users who just want to see the error. It would also be useful for values that compile successfully, to understand why a given mode was inferred and develop intuition about modes.

We did not investigate the feasibility of this feature, but it could be a powerful way to help developers build intuition about modes. We propose a complementary approach in the next section.

### Making the learning curve less steep
  - need more examples, especially for the capsule API 
  - small examples to help the devs develop the needed intuition about the modes
  - Improve error messages (link to the subpart about the priority of the error messages between typing error and mode error as this is strongly related)


###  Low-hanging fruits in the documentation 



