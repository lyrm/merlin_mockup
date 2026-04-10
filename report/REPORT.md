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

### Effect handling safety 

### ? 
OxCaml: did it help with the general design ? 
- fork/join : better design -> be more aware of what is shared (read/write) and use the right level of synchronization 


## The challenges

Portabilizing `merlin-domains` to OxCaml was a challenging experience, especially since it also includes learning OxCaml from scratch. Below, we describe the main challenges we encountered, categorized into expected and non-expected ones. As noted before, we focused our effort on leveraring OxCaml DRF guarantee to make the parallelization work, and we did not take advantage of the other features of OxCaml (e.g. locality axis and unboxed types), which is why we don't mention challenges related to these features below.

When evaluating the feasability of the project, we identified the following expected challenges:
- learning OxCaml,
- which library to use to leverage parallelism with DRF ? 
- how to guarantee DRF without changing the code vendored from OCaml typer ? 

We also encountered some non-expected challenges: 
<!-- - the complexity of the OxCaml API and the interdependency between the different features of OxCaml,  -->
<!-- Note: I add the previous part in the Learning OxCaml part. -->
- dealing with exception polymorphism,
- errors messages: understanding them but also understanding the interconnexion between type error and mode error. 
 

<!-- ### The non-expected ones

- constant changes in OxCaml API 

- error messages about mode before type (and the priority of the error messages)

- the need to understand almost everything related to modes / the interdependency between the different features of OxCaml to be able to do anything in OxCaml
    - all the modes are required to do multicore programming in OxCaml (and not just portable and contented)
    - modalities (that are kind of an exception to mode being deep (TODO check this affirmation))
    - kinds
    - unboxed types (used in the capsule API)

- exception polymorphism -->


### Learning OxCaml

#### Context
 We had no prior practical experience with OxCaml, and we had to learn it from scratch while portabilizing `merlin-domains`. This was a significant challenge, as OxCaml has a steep learning curve. 

#### Challenge
We encountered two main challenges in learning OxCaml: understanding modes, and navigating the capsule API.

The first challenge was the diversity of modes. Each mode axis has its own logic, and there are many of them. This makes it difficult to develop a good intuition for when and how to use them. In practice, learning often involves much trial and error, or going back and forth between the code and the [documentation](https://oxcaml.org/documentation/modes/intro/) to understand why a given mode is inferred and how to satisfy its constraint. Building familiarity takes time.

The second challenge was navigating the capsule API, which was the more significant challenge of the two, for several reasons:

- *Size.* The API surface is large, making it hard to know which part to focus on when starting out.

- *Fragmentation.* The API is exposed through many libraries: [`capsule0`](https://github.com/janestreet/capsule0), [`capsule`](https://github.com/janestreet/capsule), [`await`](https://github.com/janestreet/await), `portable`, `core`. Each provides a different subset or wrapping of the same underlying types. Although they are all mostly* compatible, they do not expose the same functions, which makes it hard to know where to look for what one needs, especially combined with the size of these APIs.

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

- *Mode prerequisites.* Using capsules effectively requires understanding almost all mode axes: portability and contention for DRF, but also contention for access, locality for password, uniqueness for key, and portability and linearity for the callbacks crossing capsule boundaries. This means one cannot learn how to ensure DRF in isolation if any sharing is required. A broad understanding of the mode system is a prerequisite.

- *Lack of guided material.* The API is well documented in its `.mli` files, but reference documentation alone is not enough to build an intuition for when to use each way of opening a capsule (access, password, key). More guided material, like tutorials or annotated examples showing why one approach is needed over another, would have made a real difference. 
<!-- more explanation -->

- *Verbosity.* The API requires a lot of boilerplate, which tends to hid the core logic and make it harder to understand what is going on, especially for concurrent algorithms. Here is an example, extracted from the message-passing data structure we implemented for `merlin-domains`, of how the API can make simple logic look more complex:

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
```

The boilerplate code required to use the capsule API makes it harder to understand the core logic of the function, even for this very simple example. 

#### Approach
To learn OxCaml, the tutorials on [oxcaml.org](https://oxcaml.org/documentation/tutorials/01-intro-to-parallelism-part-1/) were a great help, as well as discussions with the JS team (special thanks to Liam and Aspen). To use the capsule API, we end up doing a lot of trial and errors as it is lacking some tutorials of its own.

<!-- Example about predefining a type k -->


<!-- TODO: too negative  -->

#### Take-away
The documentation is pretty good for a lot of subjects, but the learning curve is still steep. We had to read a lot and have many discussions to understand what we were doing wrong. This is expected for a new language, but we think this could be helped with a different approach for tutorials and better editor support. This is discussed below in **LINK**.
<!-- *TODO link*. -->


### Finding the right parallelism primitive

<!-- TODO: Concurrent - spawn domain but report. -->

#### Context
 `merlin-domains` uses a two-domain architecture with a message-passing coordination mechanism: the main domain handles request dispatching while a dedicated secondary domain performs typing (and actually the computation of the whole merlin pipeline). The two domains communicate through a shared synchronization structure using a blocking send/acknowledge protocol. This is not a scheduler in the traditional sense but rather a fixed assignment of roles with explicit inter-domain communication and synchronization.

Here is the general structure of the main domain and the secondary domain (typer) in `merlin-domains`:
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
             Hermes.send_and_wait hermes (Config cfg);
             (* Does more stuff and answer the request *)
             let r = run config hermes in
             answer r)
   with _ -> ());

  Hermes.send_and_wait hermes (Msg `Closing);
  Domain.join typer
```


#### Challenge
We were planning to use `Parallel.fork_join2` and to rework our design to fit its fork/join model. However this scheduler does not garantee true parallelism: the scheduler may run tasks as fibers on the same domain. This is a problem because our message-passing data structure uses blocking acknowledgement, which would lead to deadlocks if the two tasks are run on the same domain.

Extract of the message passing data structure written in OCaml. 
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
`send_and_wait` can't progress without calling another domain calling `Condition.signal` in parallel, which is not guaranteed with `Parallel.fork_join2`.

`Concurrent` will work but will not provided any additional value over `Multicore` as the api provided `spawn_join` functions. It would also add a dependency. 

#### Approach
The best match we found for our use case is `Multicore` library, which did not require any change in the multicore design of `merlin-domains` and provided true parallelism.

<!-- 
```ocaml
let () =
  (* Hermes is a data structure used to share data in between domains *)
  let hermes = Hermes.create (fun () -> None) in
  let counter = Atomic.make 0 in

  let ( let* ) spawn_result f =
    match spawn_result with Multicore.Spawned -> f () | _ -> assert false
  in

  let* () =
    Multicore.spawn
      (fun () ->
        let rec loop () =
          match Hermes.recv_clear hermes with
          | Config config ->
              (* some computation *)
              process config hermes;
              loop ()
          | Msg `Closing -> ()
        in
        loop ();
        Atomic.incr counter)
      ()
  in

  (try
     Server.listen ~handle:(fun req ->
         match req with
         | Server.Close -> raise Closing
         | Server.Config config ->
             (* Send work to the typer domain *)
             Hermes.send_and_wait hermes (Config cfg);
             (* Does more stuff and answer the request *)
             let r = run config hermes in
             answer r)
   with _ -> ());

  Hermes.send_and_wait hermes (Msg `Closing);
  Atomic.incr counter;

  (* Joining *)
  while Atomic.get counter <> 2 do
    Thread.yield ()
  done
``` 
-->

#### Take-away

- There are very little documentation about `Concurrent` and `Multicore` libraries, or about the limitation of `Parallel`. Only `Parallel` is mentioned in the oxcaml.org documentation. This should be improved to help users make the right choice for their use case. 

- Using a `fork-join` function would have help us refine: (1) when parallelisation is needed and possible and, (2) what has to be shared between the two domains at each fork.  <!-- TODO : more explanation -->


### Integrating vendored code

#### Context

Dealing with vendored code was one of the main reasons we portabilized the mock-up rather than the real project: we needed a simpler codebase to explore possible approaches.

`merlin` vendors a large portion of the OCaml compiler, in particular the typer (`src/ocaml/`). This vendored code is written in plain OCaml, was designed for a single-threaded world, and is full of mutable state: roughly 40+ module-level refs and mutable record fields on type nodes. This vendored code cannot be rewritten in OxCaml: it is rebased onto each new OCaml release, and any modification would have to be redone at every rebase.

At the same time, both domains call into vendored code. The worker domain runs the typer, and the main domain runs analysis code (calling functions like `Ctype.unify`, `Printtyp.wrap_printing_env`, `Env.find_type`, etc.) on the partial result. During the partial result scenario, both domains execute vendored code concurrently, creating data races on shared mutable state.

This situation is not specific to merlin: any project that depends on non-OxCaml libraries faces the same question. The vendored code can't benefit from OxCaml's mode system directly, but we still want the rest of the codebase to get some DRF guarantees when interfacing with it.

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
  (* Hidden global state — not visible in the signature *)
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

This makes the functions callable from both domains, but nothing prevents two domains from calling them concurrently. Both `add_entry` and `compute` mutate hidden global state (`global_counter`), and `add_entry` also mutates `env` — so calling any combination of these from two domains in parallel is a data race. The naive wrapper compiles, but it is no safer than plain OCaml.

We needed to find a way to leverage OxCaml's mode system to enforce mutual exclusion on vendored function calls at compile time, without being able to change the vendored code itself.

#### Approach

Our approach is to require a branded `Capsule.Access.t` to call any wrapper function. We create a single capsule/mutex pair and tie the wrapper to its brand `k`. We also wrap the visible mutable state (`Vendored.env`) in a `Capsule.Data.t`, so that unwrapping it requires the same `Access.t`:

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

For `add_entry`, the `Access.t` serves a double purpose: it is needed both to unwrap `env` from its `Capsule.Data.t` and to authorize the call (which mutates hidden global state). For `compute`, which does not take an `env` argument, the `Access.t` only serves as an authorization token: it is not structurally needed, but requiring it ensures the caller holds the mutex. In both cases, the wrapper is the trust boundary: `Obj.magic_portable` casts are used inside, and the wrapper author must verify that the vendored functions are safe to call under the lock. Outside the wrapper, the compiler enforces the discipline: no `Access.t`, no call.

On the caller side, this looks like:

```ocaml
let await = Await_blocking.await Terminator.never in
Mutex.with_key await Lock.mutex ~f:(fun key ->
    Capsule.Expert.Key.access key ~f:(fun access ->
        Wrapper.add_entry ~access env "hello";
        let _ = Wrapper.compute ~access 42 in
        ()))
```

A single mutex does not mean a single big critical section: the caller controls the granularity of each acquisition, and parallelism comes from the gaps between them.

We chose a single mutex rather than multiple mutexes (one per category of state) because vendored functions often touch multiple categories of state in a single call. For instance, `Ctype.unify` mutates `trail`, `current_level`, `type_expr` fields, and `abbreviations` all at once. Multiple mutexes would require acquiring several locks atomically, introducing deadlock risks. A single mutex avoids lock ordering issues entirely.

<!-- The full example with the Vendored, Wrapper and test code is in
     report/vendored_code_example.ml -->

#### Take-away

- *The branded `Access.t` pattern is a general approach* for interfacing OxCaml code with non-OxCaml dependencies: the compiler enforces the mutex discipline at the boundary, without modifying the dependency.

- *The guarantee is only as good as the wrapper*: a vendored function missing from the wrapper can be called without the mutex. The wrapper must be audited manually.

- *The wrapper maintenance cost seems acceptable for merlin*: it only needs updating when the vendored API surface changes (new or modified function signatures), not for internal refactors.

- *Open question*: is wrapping visible state in `Capsule.Data.t` worth the added verbosity? The `Access.t` token already forces the mutex, so the structural protection it adds seems marginal.


### Dealing with exception polymorphism

#### Context

#### Challenge

#### Approach

#### Take-away

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
  because it closes over the value "g" at line 7, characters 13-14
    which is "nonportable"
    because it contains a usage (of the value "state" at line 6, characters 12-17)
      which is expected to be "uncontended".
  However, the value "h" highlighted is expected to be "shareable"
    because it is used inside the function at line 9, characters 4-22
      which is expected to be "shareable".
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



