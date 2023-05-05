---
id: lima
title: Lima
description: Lima changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## API

### Deprecation

The types `chest` and the primitive `Tezos.open_chest` are deprecated (see the changelogs for the Lima protocol).

### Changes in API

#### Tezos


<SyntaxTitle syntax="pascaligo">
val create_ticket&lt;value&gt; : value -> nat -> option (ticket (value))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val create_ticket : 'value -> nat -> ('value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let create_ticket: 'value => nat => option&lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>

Now `create_ticket` returns an `option`, preventing the creation of zero valued tickets.
