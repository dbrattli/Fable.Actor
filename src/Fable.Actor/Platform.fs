/// Platform primitives for BEAM target.
///
/// Delegates to Fable.Beam.Erlang for standard BIFs and keeps only
/// actor-specific protocol Emits (tagged messages, selective receive).
///
/// decision: uses typed Fable.Beam bindings except where selective receive requires bound Erlang variables
/// invariant: public actor messages and replies retain their established Erlang envelope tags
module Fable.Actor.Platform

#if FABLE_COMPILER_BEAM

open Fable.Core
open Fable.Core.BeamInterop
open Fable.Actor.Types
open Fable.Beam

// ============================================================================
// Atom literals
// ============================================================================

let private atomKill: Atom = Erlang.binaryToAtom "kill"
let private atomNormal: Atom = Erlang.binaryToAtom "normal"

// ============================================================================
// Process helpers (use Fable.Beam.Erlang with actor-specific atoms)
// ============================================================================

let killProcess (pid: Pid<'Msg>) : unit = Erlang.exitPid pid atomKill
let trapExits () : unit = Erlang.trapExit () |> ignore
let formatReason (reason: obj) : string = Erlang.formatTerm reason

// ============================================================================
// Internal message protocol
// ============================================================================

/// Tagged-tuple envelopes used by the BEAM wire protocol.
///
/// decision: models envelopes as a DU so send and receive derive the same tags from one typed definition
/// invariant: CompiledName values match the tags consumed by native Erlang interoperability code
type InternalMsg =
    | [<CompiledName("fable_actor_msg")>] ActorMsg of payload: obj
    | [<CompiledName("fable_actor_reply")>] Reply of ref: Ref<obj> * value: obj
    | [<CompiledName("EXIT")>] Exit of pid: Pid<obj> * reason: obj

// ============================================================================
// Message passing
// ============================================================================

/// Send a tagged user message: Pid ! {fable_actor_msg, Msg}.
/// The envelope tag comes from InternalMsg.ActorMsg's CompiledName.
let sendMsg (pid: Pid<'Msg>) (msg: 'Msg) : unit =
    Erlang.send (unbox<Pid<InternalMsg>> pid) (ActorMsg(box msg))

/// Send a tagged reply: Pid ! {fable_actor_reply, Ref, Value}.
/// The envelope tag comes from InternalMsg.Reply's CompiledName.
let sendReply (pid: Pid<'Caller>) (ref: Ref<'Reply>) (value: 'Reply) : unit =
    Erlang.send (unbox<Pid<InternalMsg>> pid) (Reply(unbox<Ref<obj>> ref, box value))

/// Block until a user message or abnormal child exit is available.
///
/// decision: drops stale replies and normal exits because neither is an application message
/// invariant: abnormal EXIT signals reach the actor body as ChildExited values
let rec receiveMsg (cont: obj -> unit) : unit =
    match Erlang.receive<InternalMsg> () with
    | ActorMsg payload -> cont payload
    | Reply _ -> receiveMsg cont // stray reply (ref already timed out); drop and keep waiting
    | Exit(_, reason) when Erlang.exactEquals reason atomNormal -> receiveMsg cont
    | Exit(pid, reason) -> cont (box ({ Pid = box pid; Reason = reason }: ChildExited))

/// Block until the reply matching a specific ref arrives.
///
/// decision: emits the receive expression directly to preserve Erlang bound-variable matching semantics
/// invariant: replies with other refs remain in the mailbox
let recvReply (ref: Ref<'Reply>) : 'Reply =
    emitErlExpr ref "receive {fable_actor_reply, $0, FableReply} -> FableReply end"

/// Selectively receive a reply or return None after the timeout.
///
/// invariant: timing out does not consume a late or unrelated reply
let recvReplyWithTimeout (ref: Ref<'Reply>) (timeout: int) : 'Reply option =
    emitErlExpr (ref, timeout) "receive {fable_actor_reply, $0, FableReply} -> {some, FableReply} after $1 -> undefined end"

// ============================================================================
// Child exit detection
// ============================================================================

[<Emit("is_map($0) andalso is_map_key(pid, $0) andalso is_map_key(reason, $0)")>]
let isChildExited (msg: obj) : bool = nativeOnly

// ============================================================================
// Timer
// ============================================================================

type private TimerControl = | [<CompiledName("cancel")>] Cancel

/// Schedule a callback after ms milliseconds and return its process for cancellation.
///
/// decision: gives each timer a process so cancellation uses ordinary BEAM messaging
/// tradeoff: allocates one lightweight process per timer to avoid a shared timer registry
let timerSchedule (ms: int) (callback: unit -> unit) : obj =
    let pid: Pid<TimerControl> =
        Erlang.spawn (fun () ->
            match Erlang.receive<TimerControl> ms with
            | Some Cancel -> ()
            | None -> callback ())

    box pid

/// Cancel a scheduled timer by sending the cancel atom to its process.
let timerCancel (timer: obj) : unit =
    Erlang.send (unbox<Pid<TimerControl>> timer) Cancel

#endif
