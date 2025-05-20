---
id: kathmandu
title: Kathmandu
description: Kathmandu changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Note: as LIGO uses Mumbai protocol to Michelson type-check your programs, the flag `--disable-michelson-typechecking` is recommended to compile contracts to Kathmandu when using tickets / chest.

## API

### New primitives

#### Tezos

<SyntaxTitle syntax="cameligo">
val emit : string -> 'a -> operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let emit: string => &apos;a => operation
</SyntaxTitle>

Build an event operation. To actually emit an event, this operation must be returned the same way as other operations (origination / transfer ..)

#### Test
<SyntaxTitle syntax="cameligo">
val get_last_events_from : ('p,'s) typed_address -> string -> 'a list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_last_events_from: typed_address &lt;&apos;p,&apos;s&gt; => string => list &lt;&apos;a&gt;
</SyntaxTitle>

Returns the list of all the event payloads emited with a given tag by a given address. Any call to this
function must be annotated with the expected payload type.


## Event testing

Here is how you emit events and fetch them from your tests:

<Syntax syntax="cameligo">

```cameligo test-ligo group=test_ex
module C = struct
  [@entry]
  let main (p : int * int) () =
    let op1 = Tezos.Operation.emit "%foo" p in
    let op2 = Tezos.Operation.emit "%foo" p.0 in
    [op1; op2], ()
end

let test_foo =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let _: nat = Test.Typed_address.transfer_exn orig.taddr (Main (1,2)) 0tez in
  (Test.State.last_events orig.taddr "foo" : (int*int) list),(Test.State.last_events orig.taddr "foo" : int list)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test_ex
namespace C {
  // @entry
  const main = (p: [int, int], _: unit) : [list<operation>, unit] => {
    const op1 = Tezos.Operation.emit("%foo", p);
    const op2 = Tezos.Operation.emit("%foo", p[0]);
    return [[op1, op2], unit];
  };
}

const test = () => {
  const orig = Test.Originate.contract(contract_of(C), unit, 0 as tez);
  Test.Typed_address.transfer_exn(orig.taddr, ["Main" as "Main", [1,2]], 0 as tez);
  return [Test.State.last_events(orig.taddr, "foo") as list<[int, int]>, Test.State.last_events(orig.taddr, "foo") as list<int>];
};

const run_test = test();
```

</Syntax>
