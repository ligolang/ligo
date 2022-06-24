---
id: convert-address-to-contract
title: How to convert an `address` to a `contract` ?
---

import Syntax from '@theme/Syntax';

In the context of testing framework,
if you want to convert an address to a contract,
you need to convert `address` to `typed_address` using `Test.cast_address`.

Then convert `typed_address` to `contract` using `Test.to_contract`

Check out the reference of the `Test` framework for exact signature of the functions [here](../reference/test).
