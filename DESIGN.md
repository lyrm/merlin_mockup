# 3 Dec 2025
## Things we need to understand better
- can the analyse phase be run in parallel with the typing without a mutex
  - does it do any write on mutable values (analysis with tsan)
  - if no write, are there read on mutable modifyed by the typer (and thus non necessary bad data races)
- Have a better understanding about how the typing works (what is mutable and when it is written for example)
- How much work would it require to make the main domain listen instead of waiting during the first phase of the typing (before early type return)

## For merlin (and not the mockup)
(see next part for detail about design)

The plan in merlin will to implement either design 0 or design 1 (depending on what works better on the mockup) and if time, 
design 2.


## Possible designs for the mockup

As a reminder: it will be nice to do one of this design in Rust.

### Design 0
*General idea*: have one fork-join where the `Domain.spawn` is currently done

*Pro*: 
- really experiment the migration from OCaml to OxCaml without making it easier by changing the design (not all projetcs can do that)
- minimum changes in the parallelism design: benchmarks can be easily compared, as well as code changes

*Cons*: 
- can't work without `Obj.magic` in merlin and probably in the mockup (for the data races with the ocaml compiler)
- sharing is going to be more complex: 
  - the many global mutable variables are going to require a top-level capsule as well 
  - everything must be share/shareable before the fork-join

*Why trying this design?*
The measure / be able to document the cost of migrating without compromising the parallel design, which may happen in a lot of project. Also to try it without purposefully making it easier (see Design 2). It is also a needed reference

### Design 1 
*General idea*: have multiple fork-join but with the same way of distributing the tasks 

*Where should be the fork-joins? *
- single domain: listening
- starts after process query info step: 
  - waiting (or listening)
  - typing 
  - stops: with early type return
- (optional if no early type return occurs) starts after early type return
  - analysis
  - typing
  - stops: when analysis is over (and the answer has been sent)
- (optional if no early type return occurs or typing is over)
  - Listen
  - Typing
- then back to single domain: listening

*Pro*: 
- finer grains (than design 0) on fork-join and thus on how sharing mutables
- still same overall parallel design -> should hopefully require only localised changes

*Cons*: 
- same with design 0: some data races will required the use Obj.magic

*Why trying this design?*
It seems like a better use of fork-join model than Design 0. Comparison between the 2 implementations (or the attempt of  implementing) will be quite interesting.

### Design 2 
*General idea*: 
(New design) One domain is only doing the listening part (+ later on the cancellation mechanism) and the other domain does everything else 

*More detail on the implementation for later*

*Pro*
- this design avoids sharing almost completly and thus should more naturally work with OxCaml
- still a good design of OCaml if the analysis can't be run in parallel with the typing
  
*Cons*
- major redesign requires, that should be first done in OCaml/OCaml
