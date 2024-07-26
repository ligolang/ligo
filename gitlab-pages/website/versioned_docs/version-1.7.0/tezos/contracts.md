---
id: contracts-type
title: Contracts
---

import Syntax from '@theme/Syntax';

In order to handle addresses that denote an originated account on the
chain, we need a value of type `contract`. In fact, it is a type
parameterised by the type of the contract's parameter. Contrary to the
type `address`, there are no literal values of the type `contract`, so
values have to be created by means of predefined functions.

<Syntax syntax="cameligo">

The call `Tezos.implicit_account kh` casts the public key hash `kh`
into the address of its *implicit account*. Note that addresses of
implicit accounts always have the type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.implicit_account(kh)` casts the public key hash `kh`
into the address of its *implicit account*. Note that addresses of
implicit accounts always have the type `contract<unit>`.

</Syntax>