---
id: current-reference
title: Current - Things relating to the current execution context
---

## Current.balance() : tez

Get the balance for the contract.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const p : unit; const s: tez) : list(operation) * storage is
  ((nil : list(operation)), balance)
```
<!--CameLIGO-->
```cameligo
let main (p, s : unit * storage) =
  ([] : operation list), balance
```
<!--ReasonLIGO-->
```reasonligo
let main = (p: unit, storage) => ([]: list(operation), balance);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.time() : timestamp

Returns the current time as a [unix timestamp](https://en.wikipedia.org/wiki/Unix_time). 

In LIGO, timestamps are type compatible in operations with `int`(s). This lets you set e.g. time constraints for your smart contracts like this:

### Examples

#### 24 hours from now
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
const today: timestamp = now;
const one_day: int = 86400;
const in_24_hrs: timestamp = today + one_day;
const some_date: timestamp = ("2000-01-01T10:10:10Z" : timestamp);
const one_day_later: timestamp = some_date + one_day;
```

<!--CameLIGO-->
```cameligo group=b
let today: timestamp = Current.time
let one_day: int = 86400
let in_24_hrs: timestamp = today + one_day
let some_date: timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later: timestamp = some_date + one_day
```

<!--ReasonLIGO-->
```reasonligo group=b
let today: timestamp = Current.time;
let one_day: int = 86400;
let in_24_hrs: timestamp = today + one_day;
let some_date: timestamp = ("2000-01-01t10:10:10Z" : timestamp);
let one_day_later: timestamp = some_date + one_day;
```

<!--END_DOCUSAURUS_CODE_TABS-->

#### 24 hours ago
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=c
const today: timestamp = now;
const one_day: int = 86400;
const in_24_hrs: timestamp = today - one_day;
```

<!--CameLIGO-->
```cameligo group=c
let today: timestamp = Current.time
let one_day: int = 86400
let in_24_hrs: timestamp = today - one_day
```

<!--ReasonLIGO-->
```reasonligo group=c
let today: timestamp = Current.time;
let one_day: int = 86400;
let in_24_hrs: timestamp = today - one_day;
```

<!--END_DOCUSAURUS_CODE_TABS-->

#### Comparing timestamps

You can also compare timestamps using the same comparison operators as for numbers:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=c
const not_tommorow: bool = (now = in_24_hrs)
```

<!--CameLIGO-->
```cameligo group=c
let not_tomorrow: bool = (Current.time = in_24_hrs)
```

<!--ReasonLIGO-->
```reasonligo group=c
let not_tomorrow: bool = (Current.time == in_24_hrs);
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Current.amount() : tez

Get the amount of tez provided by the sender to complete this transaction.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function check (const p: unit) : int is
  begin
    var result : int := 0;
    if amount = 100tz then
      result := 42
    else
      result := 0
  end with result
```

<!--CameLIGO-->
```cameligo
let check_ (p: unit) : int = if Current.amount = 100tz then 42 else 0
```

<!--ReasonLIGO-->
```reasonligo
let check_ = (p: unit) : int =>
  if (Current.amount == 100tz) {
    42;
  }
  else {
    0;
  };
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.sender() : address

Get the address that initiated the current transaction.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const p: unit) : address is sender
```

<!--CameLIGO-->
```cameligo
let main (p: unit) : address = Current.sender
```

<!--ReasonLIGO-->
```reasonligo
let main = (p: unit) : address => Current.sender;
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.address(c: a' contract) : address

Get the address associated with a `contract`.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const p : key_hash) : address is block {
  const c : contract(unit) = implicit_account(p) ;
} with address(c)
```

<!--CameLIGO-->
```cameligo
let main (p : key_hash) =
  let c : unit contract = Current.implicit_account p in 
  Current.address c
```

<!--ReasonLIGO-->
```reasonligo
let main = (p : key_hash) : address => {
  let c : contract(unit) = Current.implicit_account(p) ;
  Current.address(c) ;
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.self_address() : address

Get the address of the currently running contract.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const p: unit) : address is self_address
```

<!--CameLIGO-->
```cameligo
let main (p: unit) : address = Current.self_address 
```

<!--ReasonLIGO-->
```reasonligo
let main = (p: unit): address => Current.self_address;
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.implicit_account(p: key_hash) : a' contract

Get the default contract associated with an on-chain keypair. This contract
doesn't execute code, instead it exists to receive money on behalf of a keys
owner.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const kh: key_hash) : contract(unit) is implicit_account(kh)
```

<!--CameLIGO-->
```cameligo
let main (kh: key_hash) : unit contract = Current.implicit_account kh
```

<!--ReasonLIGO-->
```reasonligo
let main = (kh: key_hash): contract(unit) => Current.implicit_account(kh);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.source() : address

Get the _originator_ of the current transaction. That is, if a chain of transactions
led to the current execution get the address that began the chain. Not to be confused
with `Current.sender`, which gives the address of the contract or user which directly
caused the current transaction.

> ⚠️
> There are a few caveats you should keep in mind before using `SOURCE` over `SENDER`:
>
> 1. SOURCE will never be a contract, so if you want to allow contracts (multisigs etc) to operate your contract, you need to use SENDER
> 2. https://vessenes.com/tx-origin-and-ethereum-oh-my/ -- in general it is somewhat unsafe to assume that SOURCE understands everything that's going to happen in a transaction. If SOURCE transfers to a malicious (or sufficiently attackable) contract, that contract might potentially transfer to yours, without SOURCE's consent. So if you are using SOURCE for authentication, you risk being confused. A good historical example of this is bakers paying out delegation rewards. Naive bakers did (and probably still do) just use tezos-client to transfer to whatever KT1 delegates they had, even if those KT1 were malicious scripts.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const p: unit) : address is source
```

<!--CameLIGO-->
```cameligo
let main (p: unit) : address = Current.source
```

<!--ReasonLIGO-->
```reasonligo
let main = (p: unit) : address => Current.source;
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Current.failwith(error_message: string) : a'

Cause the contract to fail with an error message.

> ⚠ Using this currently requires a type annotation on the failwith to unify it
> with the type of whatever other code branch it's on.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function main (const p : param; const s : unit) : list(operation) * unit is
  block {
    case p of
    | Zero (n) -> if n > 0n then failwith("fail") else skip
    | Pos (n) -> if n > 0n then skip else failwith("fail")
    end
  }
  with ((nil : list(operation)), s)
```

<!--CameLIGO-->
```cameligo
let main (p: unit) storage =
  if true then failwith "This contract always fails" else ()
```

<!--ReasonLIGO-->
```reasonligo
let main = (p: unit, storage) =>
  if (true) {
    failwith("This contract always fails");
  } else {
    ();
  };
```

<!--END_DOCUSAURUS_CODE_TABS-->
