module Fable.Actor.Tests.Main

open type Scriptorium.Quill.Runner

// .NET runner. Unlike the Fable targets this is a plain `dotnet run` — the non-BEAM Actor
// implementation is MailboxProcessor-based, so the same suite is a real behavioral run here and
// not just a compile smoke test. Quill blocks on Async.RunSynchronously and returns the exit code.
[<EntryPoint>]
let main _ =
    runTests [ ActorTests.tests; SupervisionTests.tests; BuilderTests.tests ]
