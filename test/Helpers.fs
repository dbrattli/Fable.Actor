namespace Fable.Actor.Tests

open Fable.Actor
open Fable.Actor.Types

// Shared plumbing for the cross-target behavioral suite. Assertions and the runner come from
// Scriptorium (Nib + Quill); what is left here is the glue the suite needs to drive actors —
// a per-target sleep, the bridge that hands an `ActorOp` to Quill (which speaks `Async`), and a
// reporter actor for observing state that has to cross a process boundary on BEAM.
//
// decision: keeps one behavioral suite for every target so platform branches must satisfy the same API contract
// invariant: target-specific test plumbing remains contained in this module
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

    /// Bridge an ActorOp into the Async that Quill expects.
    ///
    /// decision: runs the CPS continuation inline because BEAM Async is erased to synchronous callbacks
    /// assumption: the operation completes synchronously on BEAM before the returned Async completes
    let toAsync (op: ActorOp<unit>) : Async<unit> = async { op.Run(fun () -> ()) }

#else

    /// Suspend the current actor for `ms` milliseconds (yields to the event loop).
    let sleep (ms: int) : ActorOp<unit> = Async.Sleep ms

    /// On every target but BEAM `ActorOp` *is* `Async`, so the bridge is the identity.
    let toAsync (op: ActorOp<unit>) : Async<unit> = op

#endif

    /// A one-cell actor for observing state across a process boundary.
    ///
    /// decision: reports observations through messages because BEAM actors copy captured mutable values
    /// invariant: Some replaces the cell and None replies with its latest value
    let reporter initial =
        Actor.start initial (fun state (msg, rc) ->
            match msg with
            | Some v -> Continue v
            | None ->
                rc.Reply state
                Continue state)
