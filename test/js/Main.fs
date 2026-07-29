module Fable.Actor.Tests.Main

open type Scriptorium.Quill.Runner

// JS/Node runner. On this target Quill cannot block, so `runTests` returns 0 immediately and
// chains `process.exit` onto the resolved promise itself — the value returned from here is
// ignored, and no wrapper script is needed.
[<EntryPoint>]
let main _ =
    runTests [ ActorTests.tests; SupervisionTests.tests; BuilderTests.tests ]
