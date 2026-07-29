namespace Fable.Actor.Tests

open Fable.Actor
open Fable.Actor.Types

// Shared plumbing for the cross-target behavioral suite. Assertions and the runner come from
// Scriptorium (Nib + Quill); what is left here is the glue the suite needs to drive actors —
// a per-target sleep, the bridge that hands an `ActorOp` to Quill (which speaks `Async`), and a
// reporter actor for observing state that has to cross a process boundary on BEAM.
[<AutoOpen>]
module Helpers =

    /// Counter protocol, shared by the call/reply and callAsync suites.
    type CounterMsg =
        | Increment
        | Decrement
        | GetCount

#if FABLE_COMPILER_BEAM
    open Fable.Core

    [<Emit("timer:sleep($0)")>]
    let private sleepMs (ms: int) : unit = nativeOnly

    /// Suspend the current actor for `ms` milliseconds.
    let sleep (ms: int) : ActorOp<unit> =
        actor {
            sleepMs ms
            return ()
        }

    /// Bridge an `ActorOp` into the `Async` that Quill's `testAsync` expects. On BEAM `ActorOp`
    /// is a CPS record and `Async` is erased to synchronous callbacks, so running the
    /// continuation chain inline is all there is to do — there is no scheduler to hand it to.
    let toAsync (op: ActorOp<unit>) : Async<unit> = async { op.Run(fun () -> ()) }

#else

    /// Suspend the current actor for `ms` milliseconds (yields to the event loop).
    let sleep (ms: int) : ActorOp<unit> = Async.Sleep ms

    /// On every target but BEAM `ActorOp` *is* `Async`, so the bridge is the identity.
    let toAsync (op: ActorOp<unit>) : Async<unit> = op

#endif

    /// A reporter actor: a one-cell mailbox for state that has to cross a process boundary.
    /// On BEAM a `let mutable` captured by a spawned actor is a copy, so a test publishes its
    /// observation with `Actor.cast reporter (Some v)` and reads it back with
    /// `Actor.call reporter None`.
    let reporter initial =
        Actor.start initial (fun state (msg, rc) ->
            match msg with
            | Some v -> Continue v
            | None ->
                rc.Reply state
                Continue state)
