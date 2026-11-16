---
id: stdlib-mailbox
title: Mailbox Module
slug: /stdlib/mailbox
---

A mailbox lets concurrent producers send messages to a single handler. The handler processes queued messages one at a time in the order they enter the queue. Each call receives the state returned by the previous call. `Mailbox<'message, 'state>` is an opaque handle to that queue and its state.

The CLI enables Mailbox by default for script execution and compilation to .NET and C#. Hosted interpreters must grant `Threading` to access the type and all three functions. See [Host permissions](../embedding/host-permissions).

## `Mailbox.start`

```text
Mailbox.start : int -> ('state -> 'message -> 'state) -> 'state -> Mailbox<'message, 'state>
```

Creates a mailbox from its capacity, handler, and initial state. Capacity must be between 1 and 2,147,483,647. It counts messages waiting in the queue. The message currently being handled occupies no queue slot.

## `Mailbox.post`

```text
Mailbox.post : 'message -> Mailbox<'message, 'state> -> unit
```

Adds a message to the queue, waiting for a free slot if the queue is full. A successful call returns as soon as the message is queued. The handler runs asynchronously. Posts made after the mailbox closes raise an error.

## `Mailbox.finish`

```text
Mailbox.finish : Mailbox<'message, 'state> -> 'state
```

Closes the mailbox to new posts, waits for the handler to process queued messages and messages from posts already waiting for space, then returns the final state. If the handler raises an error, waiting calls wake up and the error propagates through the task scope.

## Example with concurrent producers

Await the producer tasks before calling `finish` so every producer gets a chance to post. The script returns `15`.

```fsharp
type Message =
    | Add of int
    | Reset

let handle state message =
    match message with
    | Add amount -> state + amount
    | Reset -> 0

let inbox = Mailbox.start 1 handle 0
Mailbox.post Reset inbox

let producers =
    [ Task.spawn (fun () -> Mailbox.post (Add 10) inbox)
      Task.spawn (fun () -> Mailbox.post (Add 5) inbox) ]

Task.awaitAll producers |> ignore
let result = Mailbox.finish inbox
if result = 15 then result else raise "Mailbox lost a message"
```
