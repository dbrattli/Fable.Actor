module Fable.Actor.Tests.SupervisionTests

open Scriptorium.Quill
open Scriptorium.Nib.Assertion
open type Scriptorium.Quill.Test

open Fable.Actor
open Fable.Actor.Types

// ============================================================================
// spawnLinked
// ============================================================================

let private linkTests =
    testList (
        "spawnLinked",
        [
            testAsync (
                "a child crash does not take down a parent that traps exits",
                fun _ ->
                    toAsync (
                        actor {
                            let supervisor =
                                Actor.spawn (fun inbox ->
                                    Actor.trapExits ()

                                    let child: Actor<string> =
                                        Actor.spawnLinked inbox (fun childInbox ->
                                            let rec loop () =
                                                actor {
                                                    let! _msg = childInbox.Receive()
                                                    failwith "crash!"
                                                    return! loop ()
                                                }

                                            loop ())

                                    // Send a message to make the child crash
                                    Actor.send child "boom"

                                    // Receive the EXIT signal
                                    let rec loop (crashCount: int) =
                                        actor {
                                            let! _msg = inbox.Receive()
                                            return! loop (crashCount + 1)
                                        }

                                    loop 0)

                            do! sleep 100
                            // Getting here without the supervisor crashing means supervision works
                            assertThat true isTrue
                        }
                    )
            )
        ]
    )

// ============================================================================
// spawnSupervised
// ============================================================================

let private supervisedTests =
    testList (
        "spawnSupervised",
        [
            testAsync (
                "OneForOne Restart restarts a crashed child",
                fun _ ->
                    toAsync (
                        actor {
                            let restarts = reporter 0

                            let _parent: Actor<obj> =
                                Actor.spawn (fun inbox ->
                                    Actor.trapExits ()

                                    let child =
                                        Actor.spawnSupervised inbox (OneForOne(fun _ex -> Directive.Restart)) (fun childInbox ->
                                            let rec loop () =
                                                actor {
                                                    let! msg = childInbox.Receive()

                                                    if msg = "crash" then
                                                        failwith "intentional crash"

                                                    return! loop ()
                                                }

                                            loop ())

                                    Actor.send child.Actor "crash"

                                    let rec loop restartCount =
                                        actor {
                                            let! msg = inbox.Receive()

                                            match Actor.tryAsChildExited msg with
                                            | Some exited ->
                                                let restarted = Actor.handleChildExit inbox child exited
                                                let newCount = if restarted then restartCount + 1 else restartCount
                                                Actor.cast restarts (Some newCount)
                                                return! loop newCount
                                            | None -> return! loop restartCount
                                        }

                                    loop 0)

                            do! sleep 200
                            let! count = Actor.call restarts None
                            assertThat count (isGreaterOrEqual 1)
                        }
                    )
            )

            testAsync (
                "OneForOne Stop leaves a crashed child stopped",
                fun _ ->
                    toAsync (
                        actor {
                            let flag = reporter false

                            let _parent: Actor<obj> =
                                Actor.spawn (fun inbox ->
                                    Actor.trapExits ()

                                    let child =
                                        Actor.spawnSupervised inbox (OneForOne(fun _ex -> Directive.Stop)) (fun childInbox ->
                                            let rec loop () =
                                                actor {
                                                    let! _msg = childInbox.Receive()
                                                    failwith "crash!"
                                                    return! loop ()
                                                }

                                            loop ())

                                    Actor.send child.Actor "boom"

                                    let rec loop () =
                                        actor {
                                            let! msg = inbox.Receive()

                                            match Actor.tryAsChildExited msg with
                                            | Some exited ->
                                                let restarted = Actor.handleChildExit inbox child exited

                                                if not restarted then
                                                    Actor.cast flag (Some true)
                                            | None -> ()

                                            return! loop ()
                                        }

                                    loop ())

                            do! sleep 200
                            let! stopped = Actor.call flag None
                            assertThat stopped isTrue
                        }
                    )
            )
        ]
    )

// ============================================================================
// StopAbnormal
// ============================================================================

let private stopAbnormalTests =
    testList (
        "StopAbnormal",
        [
            testAsync (
                "a raised ProcessExitException reaches the parent as ChildExited",
                fun _ ->
                    toAsync (
                        actor {
                            let flag = reporter false

                            let _parent: Actor<obj> =
                                Actor.spawn (fun inbox ->
                                    Actor.trapExits ()

                                    let stoppingChild =
                                        Actor.spawnSupervised inbox (OneForOne(fun _ex -> Directive.Stop)) (fun childInbox ->
                                            let rec loop () =
                                                actor {
                                                    let! msg = childInbox.Receive()

                                                    if msg = "stop-abnormal" then
                                                        raise (ProcessExitException "intentional abnormal stop")

                                                    return! loop ()
                                                }

                                            loop ())

                                    Actor.send stoppingChild.Actor "stop-abnormal"

                                    let rec loop () =
                                        actor {
                                            let! msg = inbox.Receive()

                                            match Actor.tryAsChildExited msg with
                                            | Some exited ->
                                                Actor.handleChildExit inbox stoppingChild exited
                                                |> ignore

                                                Actor.cast flag (Some true)
                                            | None -> ()

                                            return! loop ()
                                        }

                                    loop ())

                            do! sleep 200
                            let! gotExit = Actor.call flag None
                            assertThat gotExit isTrue
                        }
                    )
            )

            testAsync (
                "an abnormal stop from a stateful loop propagates as ChildExited",
                fun _ ->
                    toAsync (
                        actor {
                            let flag = reporter false

                            let _parent: Actor<obj> =
                                Actor.spawn (fun inbox ->
                                    Actor.trapExits ()

                                    let child =
                                        Actor.spawnSupervised inbox (OneForOne(fun _ex -> Directive.Stop)) (fun childInbox ->
                                            let rec loop (state: int) =
                                                actor {
                                                    let! msg = childInbox.Receive()

                                                    match msg with
                                                    | "fail" -> raise (ProcessExitException "bad state")
                                                    | _ -> return! loop (state + 1)
                                                }

                                            loop 0)

                                    Actor.send child.Actor "fail"

                                    let rec loop () =
                                        actor {
                                            let! msg = inbox.Receive()

                                            match Actor.tryAsChildExited msg with
                                            | Some exited ->
                                                Actor.handleChildExit inbox child exited |> ignore
                                                Actor.cast flag (Some true)
                                            | None -> ()

                                            return! loop ()
                                        }

                                    loop ())

                            do! sleep 200
                            let! gotExit = Actor.call flag None
                            assertThat gotExit isTrue
                        }
                    )
            )

            testAsync (
                "a handler returning StopAbnormal triggers supervision",
                fun _ ->
                    toAsync (
                        actor {
                            let flag = reporter false

                            let _parent: Actor<obj> =
                                Actor.spawn (fun inbox ->
                                    Actor.trapExits ()

                                    let child =
                                        Actor.spawnSupervised inbox (OneForOne(fun _ex -> Directive.Restart)) (fun childInbox ->
                                            let handler state msg =
                                                match msg with
                                                | "crash" -> StopAbnormal(ProcessExitException "handler decided to crash")
                                                | _ -> Continue(state + 1)

                                            let rec loop state =
                                                actor {
                                                    let! msg = childInbox.Receive()

                                                    match handler state msg with
                                                    | Continue newState -> return! loop newState
                                                    | Stop -> ()
                                                    | StopAbnormal ex -> raise ex
                                                }

                                            loop 0)

                                    Actor.send child.Actor "crash"

                                    let rec loop () =
                                        actor {
                                            let! msg = inbox.Receive()

                                            match Actor.tryAsChildExited msg with
                                            | Some exited ->
                                                Actor.handleChildExit inbox child exited |> ignore
                                                Actor.cast flag (Some true)
                                            | None -> ()

                                            return! loop ()
                                        }

                                    loop ())

                            do! sleep 200
                            let! gotExit = Actor.call flag None
                            assertThat gotExit isTrue
                        }
                    )
            )
        ]
    )

let tests =
    testList ("Supervision", [ linkTests; supervisedTests; stopAbnormalTests ])
