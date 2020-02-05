---
id: current-reference
title: Current
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

## Current.gas()

## Current.sender() : address

Get the address that initiated the current transaction.

## Current.address

## Current.self_address

## Current.implicit_account

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

## Current.source

## Current.failwith

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
