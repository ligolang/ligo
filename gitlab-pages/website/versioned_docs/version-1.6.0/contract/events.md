---
id: events
title: Events
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

The call `Tezos.emit event_tag event_type` evaluates in an operation
that will write an event into the transaction receipt after the
successful execution of this contract. The event is annotated by the
string `event_tag` if it is not empty. The argument `event_type` is
used only to specify the type of data attachment.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.emit(event_tag, event_type)` evaluates in an operation
that will write an event into the transaction receipt after the
successful execution of this contract. The event is annotated by the
string `event_tag` if it is not empty. The argument `event_type` is
used only to specify the type of data attachment.

</Syntax>

To actually emit an event, this operation must be returned the same
way as other operations, like an origination or a transfer.

The first string argument must use the following notation:
* "%bar" is expected for entrypoint "Bar"
* "%default" when no entrypoint is used.
* Valid characters in annotation: ('a'..'z' | 'A'..'Z' | '_' | '.' | '%' | '@' | '0'..'9').
