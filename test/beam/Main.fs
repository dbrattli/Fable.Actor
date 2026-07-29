module Fable.Actor.Tests.Main

open type Scriptorium.Quill.Runner

// BEAM runner. Quill runs the suite synchronously here and calls `halt/1` with the exit code, so
// `erl` returns non-zero when tests fail. Fable namespaces generated BEAM modules and emits a
// `main.erl` shim that dispatches to [<EntryPoint>], which is what `erl -eval 'main:main([])'`
// calls.
[<EntryPoint>]
let main _ =
    runTests [ ActorTests.tests; SupervisionTests.tests; BuilderTests.tests ]
