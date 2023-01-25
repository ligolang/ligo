---
id: kathmandu
title: Kathmandu
description: Kathmandu changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Note: as LIGO uses Lima protocol to Michelson type-check your programs, the flag `--disable-michelson-typechecking` is recommended to compile contracts to Kathmandu when using tickets / chest.

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
let main (p,_ : (int*int) * unit ) =
  [Tezos.emit "%foo" p ; Tezos.emit "%foo" p.0],()

let test_foo =
  let (ta, _, _) = Test.originate main () 0tez in
  let _ = Test.transfer_to_contract_exn (Test.to_contract ta) (1,2) 0tez in
  (Test.get_last_events_from ta "foo" : (int*int) list),(Test.get_last_events_from ta "foo" : int list)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test_ex
let main = ([p, _] : [[int, int], unit]) => {
  let op1 = Tezos.emit("%foo", p);
  let op2 = Tezos.emit("%foo", p[0]);
  return [list([op1, op2]), unit];
  };

let test = (() : [list<[int,int]>, list<int>] => {
  let [ta, _, _] = Test.originate(main, unit, 0 as tez);
  let _ = Test.transfer_to_contract_exn(Test.to_contract(ta), [1,2], 0 as tez);
  return [Test.get_last_events_from(ta, "foo") as list<[int, int]>, Test.get_last_events_from(ta, "foo") as list<int>];
}) ();
```

</Syntax>
