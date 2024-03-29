---
id: views
title: LIGO views
description: Views for Tezos written in LIGO
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## Defining On-Chain Views

On-chain views can be defined using the `@view` attribute. For more information [see here](../protocol/hangzhou.md).

<Syntax syntax="cameligo">

```cameligo group=onchainviews
type storage = string
type ret = operation list * storage

[@entry]
let main (word : string) (store : storage) : ret
  = [] , store ^ " " ^ word

(* view 'view1', simply returns the storage *)
[@view] let view1 (() : unit) (s : storage) : storage
  = s

(* view 'v2', returns true if the storage has a given length *)
[@view] let v2 (expected_length : nat) (s : storage) : bool
  = (String.length s = expected_length)

(* view 'v3' does not use its parameters and returns a constant int *)
[@view] let v3 (() : unit) (_ : storage) : int
  = 42
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=onchainviews
type storage = string
type ret = [list<operation>, storage];

const main = (word : string, store : storage) : ret
  => [list([]) , store + " " + word]

/* view 'view1', simply returns the storage */
@view
const view1 = (_arg : unit, s : storage) : storage
  => s;

/* view 'v2', returns true if the storage has a given length */
@view
const v2 = (expected_length : nat , s : storage) : bool
  => (String.length (s) == expected_length);

/* view 'view3' does not use its parameters and returns a constant int */
@view
const view3 = (_arg : unit , _s : storage) : int
  => 42;
```

</Syntax>

## Calling On-Chain Views

<SyntaxTitle syntax="cameligo">
val call_view : string -> 'arg -> address -> 'ret option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let call_view : string => 'arg => address => option &lt;'ret&gt;
</SyntaxTitle>

The primitive `Tezos.call_view` will allow you to call another contract view and get its result by providing the view name; the contract address and the parameter of the view. If the address is nonexistent; the name does not match of of the contract
view or the parameter type do not match, `Tezos.call_view` will return `None`.

## Defining Off-Chain Views

In addition to on-chain views, LIGO views can be compiled and used as off-chain views (e.g. to be placed in metadata).

To compile an expression as a off-chain view, we can use the LIGO sub-command `compile expression`, passing the `--function-body` flag. Moreover, the `--init-file` argument can be passed to
re-use expressions from a file.

For example, if we have the following `off_chain` file containing a contract `C`:

<Syntax syntax="cameligo">

```cameligo group=view_file
module C = struct
  type storage = string

  [@entry] let append (a : string) (s : storage) : operation list * storage = [] , s ^ a

  [@entry] let clear (_ : unit) (_ : storage) : operation list * storage = [] , ""

  let v (expected_length: nat) (s: storage) : bool = (String.length s = expected_length)
end
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=view_file
namespace C {
  type storage = string

  @entry
  const append = (a: string, s: storage) : [list<operation> , storage] => [list([]), s + a];

  @entry
  const clear = (_p: unit, _s: storage) : [list<operation>, storage] => [list([]), ""];

  export const v = (expected_length: nat, s: storage) : bool => (String.length (s) == expected_length);
}
```

</Syntax>

We can compile function `v` from contract `C` as an off-chain view as follows:

<Syntax syntax="cameligo">

Input
```bash
❯ ligo compile expression cameligo "C.v" --init-file off_chain.mligo --function-body
```
Output
```bash
{ UNPAIR ; SWAP ; SIZE ; COMPARE ; EQ }
```

</Syntax>
<Syntax syntax="jsligo">

Input
```bash
❯ ligo compile expression jsligo "C.v" --init-file off_chain.jsligo --function-body
```
Output
```bash
{ UNPAIR ; SWAP ; SIZE ; COMPARE ; EQ }
```

</Syntax>

Notice that `v` is not a contract entry of `C` (no `@entry`) nor a on-chain view (no `@view`), it is just a function declared in the context of the contract, which can be used as an off-chain view.

<!-- updated use of entry -->
