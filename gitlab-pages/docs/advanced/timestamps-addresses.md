---
id: timestamps-addresses
title: Timestamps, Addresses
---

## Timestamps

LIGO features timestamps, as Michelson does, while bakers baking the
block (including the transaction in a block) are responsible for
providing the given current timestamp for the contract.

### Current Time

You can obtain the current time using the built-in syntax specific
expression, please be aware that it is up to the baker to set the
current timestamp value.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
const today : timestamp = now
```

<!--CameLIGO-->
```cameligo group=a
let today : timestamp = Current.time
```

<!--ReasonLIGO-->
```reasonligo group=a
let today : timestamp = Current.time;
```

<!--END_DOCUSAURUS_CODE_TABS-->

> When running code, the LIGO CLI option
> `--predecessor-timestamp` allows you to control what `now` returns.

### Timestamp Arithmetics

In LIGO, timestamps can be added to integers, allowing you to set time
constraints on your smart contracts. Consider the following scenarios.

#### In 24 hours

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=b
const today : timestamp = now
const one_day : int = 86400
const in_24_hrs : timestamp = today + one_day
const some_date : timestamp = ("2000-01-01T10:10:10Z" : timestamp)
const one_day_later : timestamp = some_date + one_day
```

<!--CameLIGO-->
```cameligo group=b
let today : timestamp = Current.time
let one_day : int = 86400
let in_24_hrs : timestamp = today + one_day
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later : timestamp = some_date + one_day
```

<!--ReasonLIGO-->
```reasonligo group=b
let today : timestamp = Current.time;
let one_day : int = 86400;
let in_24_hrs : timestamp = today + one_day;
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp);
let one_day_later : timestamp = some_date + one_day;
```

<!--END_DOCUSAURUS_CODE_TABS-->

#### 24 hours Ago

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
const today : timestamp = now
const one_day : int = 86400
const in_24_hrs : timestamp = today - one_day
```

<!--CameLIGO-->
```cameligo group=c
let today : timestamp = Current.time
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
```

<!--ReasonLIGO-->
```reasonligo group=c
let today : timestamp = Current.time;
let one_day : int = 86400;
let in_24_hrs : timestamp = today - one_day;
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Comparing Timestamps

You can compare timestamps using the same comparison operators
applying to numbers.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
const not_tommorow : bool = (now = in_24_hrs)
```

<!--CameLIGO-->
```cameligo group=c
let not_tomorrow : bool = (Current.time = in_24_hrs)
```

<!--ReasonLIGO-->
```reasonligo group=c
let not_tomorrow : bool = (Current.time == in_24_hrs);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Addresses

The `address` type in LIGO denotes Tezos addresses (tz1, tz2, tz3,
KT1, ...). Currently, addresses are created by casting a string to the
`address` type. Beware of failures if the address is invalid. Consider
the following examples.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=d
const my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

<!--CameLIGO-->
```cameligo group=d
let my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

<!--ReasonLIGO-->
```reasonligo group=d
let my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Signatures

The `signature` type in LIGO datatype is used for Tezos signatures
(edsig, spsig). Signatures are created by casting a string. Beware of
failure if the signature is invalid.

Here is how you can define a signature:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=e
const my_sig : signature =
  ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
  signature)
```
<!--CameLIGO-->
```cameligo group=e
let my_sig : signature =
   ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
   signature)
```
<!--ReasonLIGO-->
```reasonligo group=e
let my_sig : signature =
("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
signature);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Keys

The `key` type in LIGO is used for Tezos public keys. Do not confuse
them with map keys. Keys are made by casting strings. Beware of
failure if the key is invalid.

Here is how you can define a key.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=f
const my_key : key =
("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key)
```
<!--CameLIGO-->
```cameligo group=f
let my_key : key =
  ("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key)
```
<!--ReasonLIGO-->
```reasonligo group=f
let my_key : key =
  ("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key);
```
<!--END_DOCUSAURUS_CODE_TABS-->
