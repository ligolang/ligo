---
id: views
title: On-chain views
description: View operations for Tezos
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## On Chain Views

<SyntaxTitle syntax="pascaligo">
val call_view&lt;arg,reg&gt; : string -> arg -> address -> option (ret)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val call_view : string -> 'arg -> address -> 'ret option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let call_view : string => 'arg => address => option &lt;&apos;ret&gt;
</SyntaxTitle>

The primitive `Tezos.call_view` will allow you to call another contract view and get its result by providing the view name; the contract address and the parameter of the view. If the address is nonexistent; the name does not match of of the contract
view or the parameter type do not match, `Tezos.call_view` will return `None`.
