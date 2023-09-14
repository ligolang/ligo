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
val emit : string -> 'a -> operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let emit: string => &apos;a => operation
</SyntaxTitle>

Build an event operation. To actually emit an event, this operation must be returned the same way as other operations (origination / transfer ..)
