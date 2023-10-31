---
id: events
title: Events
description: Event operations for Tezos
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## Events

<SyntaxTitle syntax="cameligo">
val Tezos.emit : string -> 'a -> operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let Tezos.emit: string => 'a => operation
</SyntaxTitle>

Build an event operation. To actually emit an event, this operation must be returned the same way as other operations (origination / transfer ..)

The first string argument must use the following notation:
* "%bar" is expected for entrypoint "Bar"
* "%default" when no entrypoint is used.
* Valid characters in annotation: ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9').

<!-- updated use of entry -->