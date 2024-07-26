---
id: events
title: Events
---

import Syntax from '@theme/Syntax';

Events are a type of internal operation on Tezos.
Smart contracts emit events and off-chain applications can listen for events to know when things happen.
Smart contracts cannot respond to events.

To create an event, call `Tezos.Next.Operation.Emit` and pass a tag for the event and the information payload for the event.
This function returns the event operation.
Then, emit the event by including it in the list of operations in the return value of the entrypoint.

For example, this entrypoint creates two events and emits them:

<Syntax syntax="cameligo">

```cameligo group=events
[@entry]
let emitEvents (_ : unit) (storage : int) : operation list * int =
  let event1 : operation = Tezos.Next.Operation.emit "%emitEvents" "hi" in
  let event2 : operation = Tezos.Next.Operation.emit "%emitEvents" 6 in
  [event1; event2], storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=events
@entry
const emitEvents = (_: unit, storage: int): [list<operation>, int] => {
  const event1: operation = Tezos.Next.Operation.emit("%emitEvents", "hi");
  const event2: operation = Tezos.Next.Operation.emit("%emitEvents", 6);
  return [[event1, event2], storage];
}
```

</Syntax>

By convention, the event tag is a percent symbol and the name of the entrypoint that emitted the event or "default" if the contract does not use entrypoints, such as `%myEntrypoint`.
However, you can use any string composed of these valid characters as the tag: ('a'..'z' | 'A'..'Z' | '_' | '.' | '%' | '@' | '0'..'9').

For more information about events and responding to them, see [Events](https://docs.tezos.com/smart-contracts/events) on docs.tezos.com.
