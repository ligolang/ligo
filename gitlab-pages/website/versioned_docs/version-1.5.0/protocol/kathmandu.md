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

<SyntaxTitle syntax="pascaligo">
val emit&lt;a&gt; :  string -> a -> operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val emit : string -> 'a -> operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let emit: string => &apos;a => operation
</SyntaxTitle>

Build an event operation. To actually emit an event, this operation must be returned the same way as other operations (origination / transfer ..)

#### Test
<SyntaxTitle syntax="pascaligo">
val get_last_events_from&lt;a,p,s&gt; : typed_address (p,s) -> string -> list (a)
</SyntaxTitle>
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

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=test_ex
function main ( const x : (int*int) * unit ) is
  (list [Tezos.emit ("%foo", x.0) ; Tezos.emit ("%foo", x.0.0)], Unit)

const test_foo = {
  const orig = Test.originate (main, Unit, 0tez) ;
  Test.transfer_exn (orig.addr, (1,2), 0tez) ;
  const x = (Test.get_last_events_from (orig.addr, "foo") : list (int*int)) ;
  const y = (Test.get_last_events_from (orig.addr, "foo") : list (int)) ;
} with (x,y)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=test_ex
module C = struct
  [@entry] let main (p: int*int) (_: unit) =
    [Tezos.emit "%foo" p ; Tezos.emit "%foo" p.0],()
end

let test_foo =
  let orig = Test.originate (contract_of C) () 0tez in
  let _ = Test.transfer_exn orig.addr (Main (1,2)) 0tez in
  (Test.get_last_events_from orig.addr "foo" : (int*int) list),(Test.get_last_events_from orig.addr "foo" : int list)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test_ex
namespace C {
  @entry
  let main = (p: [int, int], _s : unit) => { 
    let op1 = Tezos.emit("%foo", p);
    let op2 = Tezos.emit("%foo", p[0]);
    return [list([op1, op2]), unit];
  };
}

let test = (() : [list<[int,int]>, list<int>] => {
  let orig = Test.originate(contract_of(C), unit, 0 as tez);
  Test.transfer_exn(orig.addr, Main([1,2]), 0 as tez);
  return [Test.get_last_events_from(orig.addr, "foo") as list<[int, int]>, Test.get_last_events_from(orig.addr, "foo") as list<int>];
}) ();
```

</Syntax>
