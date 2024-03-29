---
id: jakarta
title: Jakarta
description: Jakarta changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## API

### New types

#### Tezos
<SyntaxTitle syntax="pascaligo">
type tx_rollup_l2_address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type tx_rollup_l2_address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type tx_rollup_l2_address
</SyntaxTitle>
A type used to identify accounts on transaction rollups’ legders. Values of type tx_rollup_l2_address are 20-byte hashes of a BLS public keys (with a string notation based of a base58 encoding, prefixed with tz4).

### New primitives

#### Tezos


<SyntaxTitle syntax="pascaligo">
val min_block_time : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val min_block_time : unit -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let min_block_time: unit => nat;
</SyntaxTitle>
Returns the current minimal time between blocks, the value is obtained from the protocol’s minimal_block_delay constant.

## Breaking changes

### Voting power

The voting power of a contract is no longer rounded to rolls. It is now instead the full staking power of the delegate, currently expressed in mutez. Though, developers should not rely on `Tezos.voting_power` to query the staking power of a contract in mutez: the value returned by `Tezos.voting_power` is still of type` nat and it should only be considered relative to `Tezos.total_voting_power`.
