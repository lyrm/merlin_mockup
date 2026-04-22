# Portabilizing a parallel OCaml codebase to OxCaml: statically preventing data races with modes. 

## Introduction 
<!-- TODO  -->

## Why DRF? 
### Data races
A data race is a specific kind of race conditions. So let's first define race conditions. 

A race condition occurs when the behavior of a software system depends on the relative timing of events, such as the execution of threads. For example, the following code snippet demonstrates a race condition: 

```ocaml
let () =
  let d1 = Domain.spawn (fun () -> print_endline "Hello from domain 1") in
  let d2 = Domain.spawn (fun () -> print_endline "Hello from domain 2") in
  Domain.join d1;
  Domain.join d2
```

The output of this code is non-deterministic, as it depends on the scheduling of the domains. It could print "Hello from domain 1" followed by "Hello from domain 2", or vice versa. This however is not a bug: the code is correct, and the behavior is expected. 

Data races are race conditions that are way more sneaky: their behavior non-only depends on some non-deterministic scheduling, but also on some out-of-control optimizations from the compiler but also from the hardware. For example, consider the following code snippet: 

```ocaml
let a = ref 0 and b = ref 0
 
let d1 () =
   a := 1; 
   !b

let d2 () =
   b := 1; 
   !a

let main () =
  let h1 = Domain.spawn d1 in
  let h2 = Domain.spawn d2 in
  Domain.join h1, Domain.join h2
```

Sequentially consistent results:
- `r1 = 0, r2 = 1` (d2 runs first)
- `r1 = 1, r2 = 0` (d1 runs first)
- `r1 = 1, r2 = 1` (any other interleaving)

Because it is a data race `r1 = 0, r2 = 0` is also possible! Here this is an hardware optimization that can cause this behavior. 

A **data race** occurs when:

1. Two or more domains run in parallel,
2. at least two access the same mutable value,
3. and at least one of them writes to it
4. without a synchronization mechanism (like locks or atomic operations).

Non-atomic mutable values in OCaml are references, mutable fields of records and arrays.

### How to avoid data races in OCaml 
Obviously, specific tools exist to detect data races. `ThreadSanitizer`,  for example, detects data races at runtime. (link to blog post and documentation)

In OCaml, there are also libraries like `qcheck-lin` and `qcheck-stm` that help by randomly generating tests and checking for linearizability. This is very useful combine with tsan that requires a lot of tests to be effective.

Not of these solutions are static however: they catch data races at run times, and as data races are non-deterministic, they can still be missed. Other ways of preventing data races is simply by ... not sharing mutable states between domains. This means strongly restricting the use cases of multicore programming in OCaml, and this is not ideal. 

## Statically preventing data races with modes in OxCaml

So let's run with a very simple example:

```ocaml
let foo () = 
    let r = ref 42 in
    let d1 = Domain.spawn (fun () -> r := 1)in 
    let d2 = Domain.spawn (fun () -> !r) in

    Domain.join d1, Domain.join d2
```
`r` is a mutable value that is shared between two domains, and at least one of them writes to it. This is a data race. In the following, we are going to try to port this code to OxCaml, and see how the mode system prevents this from compiling. Finally, in the next section, we will see how to fix this code to make it compile in OxCaml.

### Prevent data races 
- modes: portable, contention 
- 

Two mode axes are needed to ensure data race freedom: portability and contention.

#### Portability
 It states that a function (or a value carrying a function) can be used in a parallel context. For example, `Multicore.spawn` (that replaces `Domain.spawn`) requires as first argument a portable function, as it could run in a parallel context. Portability axis has 3 values: `portable`, `shareable` and `nonportable`. By default, values are `nonportable`: a `portable` annotation is needed to make them portable. 

Syntax for the portable mode: 
```ocaml
let f : (int -> int) @ portable = fun x -> x + 1

let (f @ portable) x = x + 1

(* in the signature *)
module type A = sig
  val f : int -> int @@ portable
end

(* if all the functions of a module are portable *)
module type B = sig @@ portable
  val f : int -> int
  val g : bool -> bool
end  
```

So to ensure data race freedom, all the functions that provides some form of parallelism need to take portable functions as arguments. Also for backwards compatibility reasons, `Domain.spawn` is still available in OxCaml, but it's not annotated with the right modes: it does not provide any DRF guarantee (you get a warning if using it in OxCaml). For the following example, we will first use `Multicore.spawn` and later `Parallel.fork_join2` (that also requires a portable function) that are both different ways to provide parallelism in OxCaml.

Here is the signature of `Multicore.spawn`: 
```ocaml
val spawn :  ('a : value_or_null).
     ('a @ contended once portable unique -> unit) @ once portable unyielding
  -> 'a @ contended once portable unique
  -> 'a spawn_result @ contended once portable unique
```

But we can simplify it for our need. We will use the following function:
```ocaml
val spawn: (unit -> unit) @ portable -> unit
```

#### Contention
`Contention` axis has 3 values: `contended`, `shared` and `uncontended`. This axis is only pertinent for the non-atomic mutable values, i.e. the ones that can cause a data race. When a mutable value is `contended`, both write and read operations are forbidden on it. `shared` only prevents write operations, and `uncontended` does not prevent any operation.

The default value is `uncontended`: a value becomes `contended` when it is captured by a portable function. The idea is simple: if a function is portable, it can be used in parallel of another one, so if it captures a mutable value, to prevent data race, this value should not be accessed: it becomes `contended`. We will talk about `shared` and `shareable` later. 

Let's see how this works with the following example:
```ocaml
let foo () =
  let r = ref 42 in
  (* r is a mutable value and is uncontended by default. *)

  let (work @ portable) () = r := 1 in
  (* r is contended because it is used in a portable function. 
     The `write` operation requires an `uncontended` value: this does not compile *)
  
  spawn work
```

This example does not compile because `r` is `contended` and the `write` operation requires an `uncontended` value. We could try to keep `r` uncontended by removing the `portable` annotation on `work`, but then we would not be able to use it in `spawn`.

```ocaml
let foo () = 
    let a = ref 42 in
    (* a is uncontended *)

    let work () =  a := 1 in
    (* a is uncontended and `work` is at its default value: nonportable *)

    spawn work
    (* `spawn` requires a portable function, but `work` is nonportable : this does not compile. *)
```

Okay, so you could say, that in this example, there is actually no data races: there is just a single access to `a`. However, there is no way for the `spawn` function to know about that: it just know that it may run concurrently with another function and thus prevent any risk of data race. Let's not that this is a dummy example: there is also no reason for parallelism. We could have written: 
```ocaml
let foo () =
  let r = ref 42 in

  let (work @ portable) () = r := 1 in

  spawn work;
  spawn work
```
where there is some parallelism and the error message would be the same.

And this is how modes prevent data races statically! 

#### What about `shareable` and `shared`?
Note 1: This part is a bonus, as, as explained above, `portable`/`nonportable` and `contended`/`uncontended` are sufficient to prevent data races statically. `shareable` and `shared` are more fine-grained annotations that allow to perform concurrent read operations on shared mutable values. 

Note 2: The current published version of OxCaml does not completely support `shareable` and `shared` annotations. In particular, their is no `Reference` module with the right annotations for the read operation. You will have to write your own reference module to try the following code snippets! (A solution is provided at the bottom of the article)


The idea is the following: two concurrent read operations on a mutable value is not a data race. It should then be allowed. This is the idea behind `shared` and `shareable`. A `shared` value can be read but not written, and a `shareable` function captures `shared` values.

```ocaml 
let foo () =
  let open Ref in 
  (* As explained above, for now, you need a homemade Reference module to make this work. *)

  let r = ref 42 in
  (* r is uncontended *)

  let (work @ shareable) () = !r in
  (* r is shared because it is used in a shareable function. 
     The `read` operation requires a `shared` value: this does compile *)

  spawn work
  (* Does not compile because `spawn` requires a portable function, but `work` is shareable. *)
```

But here is the difficulty: `Multicore.spawn` takes a `portable` function as argument, not a `shareable` one. Why is that? This is because it spawns a new domain, that is running a `portable` function, but then resumes executing the current domain, that can run any non-portable function, including write and read on shared mutable values, meaning data races! An example:

```ocaml
let foo () =
  let r = ref 42 in

  let (work @ portable) () = r := 1 in

  spawn work;
  r := 2
```
Whatever `work` performs a write operation on `r` or just a read operation, this is a data race: the constraint on `work` can not be weakened.

We actually need another way to provide parallelism with more control: `Parallel.fork_join2`. `fork_join2` is blocking: it runs two portable functions (possibly) in parallel and wait for both of them to finish. Here is its signature:  

```ocaml
val fork_join2
  :  t @ local
  -> (t @ local -> 'a) @ forkable local once shareable
  -> (t @ local -> 'b) @ once shareable
  -> #('a * 'b)
```

We have well contained parallelism here: only the two functions passed as arguments to `fork_join2` can run in parallel, and they are both `shareable`, meaning they can only read shared mutable values, but not write to them, which is not a data race! Let's try it:

```ocaml
let foo par =
  let open Ref in
  let r = create 0 in

  let (read @ shareable) _par = !r in

  let #(_, _) = Parallel.fork_join2 par read read in
  ()
```
There are no data race here, as the two concurrent accesses to `r` are read operations, so this should compile and it does because `read` is a `shareable` function.


On the opposite, the following code has a data race and does not compile:
```ocaml
let foo par =
  let open Ref in
  let r = create 0 in

  let (read @ shareable) _par = !r in
  let write _par = r := 1 in
  (* If we try to make `write` portable or shareable, we would get a mode error on `r` as will become respectively contended or shared but the (:=) operation requires it to be uncontended. *)

  let #(_, _) = Parallel.fork_join2 par read write in
  (* Don't compile: write is nonportable but `fork_join2` requires both functions to be shareable. *)
  ()
```

### Sharing in OxCaml
So, how can we make our simple example compile in OxCaml? Remember, we want to do perform a read and a write operation on a shared mutable value in parallel:
```ocaml
let foo () = 
    let r = ref 42 in
    let d1 = Domain.spawn (fun () -> r := 1)in 
    let d2 = Domain.spawn (fun () -> !r) in

    Domain.join d1, Domain.join d2
```
We have to prove to the compiler that there is no data race. The recipe for a data race are the following
1. Two or more domains run in parallel,
2. at least two access the same mutable value,
3. and at least one of them writes to it
4. without a synchronization mechanism (like locks or atomic operations).

1, 2 and 3 are what we are trying to do, so we need to provide a synchronization mechanism. For our example, `atomic` are the easy way to go:  we can just replace `ref` with `Atomic` and we are good to go! 

```ocaml
let foo () =
  let r = Atomic.make 0 in

  let read () = Atomic.get r |> ignore in
  let write () = Atomic.set r 1 in

  spawn read;
  spawn write
```
This compiles!

What about locks? In OxCaml, locks are actually provided through the `Capsule` API. It would take quite way more explanations that I am planning to give in this article, but the idea is the following: mutable values are enclosed in a capsule. The capsule can be shared between domains, without getting contented (meaning it is still possible to perform operations on it). It looks like it may create data races, as this allows to share mutable values between domains, but actually, it is pretty hard to open the capsule to perform operation on its contents. This is where the data race freedom is guaranteed. 

For the operations we are trying to do, we actually need the most restrictive capsule opener: a mutex. The API for mutex is not that different to the [ones in OCaml](https://ocaml.org/manual/5.4/api/Mutex.html). There is a big difference however: the mutable value must be created under the capsule, meaning it can never be accessed without acquiring the mutex first (the `with_lock` function). 

```ocaml   
let foo () =
  let open Await in
  let await = Await_blocking.await Terminator.never in
  let capsule_with_lock = Capsule.With_mutex.create (fun () -> ref 42) in
  (* The reference is created in a capsule, that can only be accessed with the mutex. *)

  let read r = !r in
  let write r v = r := v in

  spawn (fun () ->
      let _ = Capsule.With_mutex.with_lock await capsule_with_lock ~f:(fun r -> read r) in
      (* `with_lock` is equivalent to `Mutex.protect` in OCaml.*)
      ());
  spawn (fun () ->
      Capsule.With_mutex.with_lock await capsule_with_lock ~f:(fun r -> write r 1))
```

## Interfacing OCaml code in OxCaml: challenges and solutions
<!-- TODO : the idea here is the main link to the project as it is an issue we encounter trying to portabilize merlin-domain to oxcaml -->

### Description of the challenge

Some projects mix OxCaml with plain OCaml: external libraries, vendored modules, or legacy code we cannot (or do not want to) rewrite. When that code holds mutable state, we face the data race problem again, but with a twist: we cannot add mode annotations, since the code is not ours to modify.

Let's go back to our running example, but this time the `read` and `write` operations come from a vendored module we do not want to modify:

```ocaml
module Vendored : sig
  val read : unit -> int
  val write : int -> unit
end = struct
  let r = ref 42
  let read () = !r
  let write v = r := v
end
```

Replacing `ref` with `Atomic` (as we did earlier) is not an option here, since we are not allowed to touch the vendored code.

From the OxCaml side, all the functions exported by `Vendored` are `nonportable` (the default value): they cannot be called from a `portable` context (e.g. inside a `spawn` or a `fork_join2` callback). OxCaml provides an unsafe escape hatch, `Obj.magic_portable`, that would make them callable, but it's `Obj.magic`: it's cheating. Also, it gives the compiler no way to prevent two domains from calling them concurrently, so the mutable state inside `Vendored.t` would still be racy.

What we actually need is a way to leverage OxCaml's mode system to enforce mutual exclusion on vendored function calls *at compile time*, without modifying the vendored code.

### Solution

The trick is to encode the mutex discipline directly in the signature of the wrapper functions. We create a mutex, and require a token of type `k Capsule.Access.t` as an argument of every wrapper function:

```ocaml
module Lock = Capsule.Mutex.Create ()
(* [Lock] exposes two things: a fresh type [k], and a mutex of type
   [k Mutex.t]. Because [k] is fresh and tied to this mutex, a value
   of type [k Capsule.Access.t] can only be produced by acquiring it. *)
type k = Lock.k

module Wrapper : sig @@ portable
  val read : access:k Capsule.Access.t -> int
  val write : access:k Capsule.Access.t -> int -> unit
end = struct
  let read_ = Obj.magic_portable Vendored.read
  let read ~access:_ = read_ ()
  (* [access] is unused at runtime, but the caller must supply one,
     and the only way to obtain one is to hold [Lock.mutex]. *)

  let write_ = Obj.magic_portable Vendored.write
  let write ~access:_ v = write_ v
  (* Same [k] as [read]: both functions are guaranteed to run under
     the same mutex. *)
end
```

By taking `~access` as an argument, the wrapper signature forces callers to hold `Lock.mutex` before calling any of its functions — the token is a compile-time proof of authorization. `Obj.magic_portable` is still used internally to bridge the vendored code, but it is now safely contained: the wrapper is the trust boundary, and the compiler enforces the mutex discipline everywhere else.

On the caller side, concurrent access to the vendored state now looks like this:

```ocaml
let foo () =
  let open Await in
  let await = Await_blocking.await Terminator.never in

  let safe_write () =
    Mutex.with_key await Lock.mutex ~f:(fun key ->
        Capsule.Expert.Key.access key ~f:(fun access ->
            Wrapper.write ~access 1))
  in
  let safe_read () =
    Mutex.with_key await Lock.mutex ~f:(fun key ->
        Capsule.Expert.Key.access key ~f:(fun access ->
            let _ = Wrapper.read ~access in
            ()))
  in
  spawn safe_write;
  spawn safe_read
```

Both domains can call `read` and `write`, but only under the protection of the mutex: this code is data race free!

This pattern generalizes to any non-OxCaml dependency, with one important caveat: *the guarantee is only as good as the wrapper*. A function missing from the wrapper can still be called directly via `Obj.magic_portable`, bypassing the mutex. The wrapper must therefore be audited manually, ideally complemented by runtime tools like `tsan`.

## Conclusion

<!-- TODO -->

## Implemention of the `Reference` module

A simpler version for int ref: 

```ocaml
module Ref : sig @@ portable
  type t = { mutable content : int }

  val ref : int -> t
  val ( ! ) : t @ shared -> int
  val ( =: ) : t -> int -> unit
end = struct
  type t = { mutable content : int }
  let ref x = { content = x }

  let ( ! ) r = r.content

  let ( =: ) r v = r.content <- v
end

```

A more general version: 

```ocaml
module Ref : sig @@ portable
  type ('a : value mod contended shareable) t = { mutable content : 'a }


  val ref : 'a -> 'a t
  val ( ! ) : 'a t @ shared -> 'a
  val ( =: ) : 'a t -> 'a -> unit
end = struct
  type ('a : value mod contended shareable) t = { mutable content : 'a }
  let ref x = { content = x }

  let ( ! ) r = r.content

  let ( =: ) r v = r.content <- v
end
```

The additional annotation on `'a` defines the kind of `'a`. What it means is that our reference can not carry mutable values or nonportable functions. We need this restriction because of the read function that returns an `'a` and remember, we want to be able to call it from two different domains. So we could write the following: 


```ocaml
type p = { mutable x : int }

let foo () =
  let open Ref in
  let r = ref { x = 0 } in

  spawn (fun () ->
      let t = !r in
      t.x <- 1);
  spawn (fun () ->
      let t = !r in
      t.x <- 2)
```

There are two concurrent write operations on the same mutable value `t.x`, with no synchronization mechanism: this is a data race. The kind annotation statically prevents us from writing this code, as the kind of `p` is `mutable_data`: it is incompatible with our annotation, and those, this code does not compile.