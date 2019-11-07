---
id: timestamps-addresses
title: Timestamps, Addresses
---

## Timestamps 

Timestamps in LIGO, or in Michelson in general are available in smart contracts, while bakers baking the block (including the transaction in a block) are responsible for providing the given current timestamp for the contract.

### Current time

You can obtain the current time using the built-in syntax specific expression, please be aware that it's up to the baker to set the current timestamp value.


<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const today: timestamp = now;
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Timestamp arithmetic

In LIGO, timestamps can be added with `int`(s), this enables you to set e.g. time constraints for your smart contracts like this:

#### In 24 hours
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const today: timestamp = now;
const one_day: int = 86400;
const in_24_hrs: timestamp = today + one_day;
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### 24 hours ago
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const today: timestamp = now;
const one_day: int = 86400;
const 24_hrs_ago: timestamp = today - one_day;
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Comparing timestamps

You can also compare timestamps using the same comparison operators as for numbers:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const not_tommorow: bool = (now = in_24_hrs)
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Addresses

`address` is a LIGO datatype used for Tezos addresses (tz1, tz2, tz3, KT1, ...).

Here's how you can define an address:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const my_account: address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);
```
<!--END_DOCUSAURUS_CODE_TABS-->

