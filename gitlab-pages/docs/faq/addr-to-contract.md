---
id: convert-address-to-contract
title: How to convert an address to a contract in LIGO testing framework ?
---

import Syntax from '@theme/Syntax';

In the context of testing framework, if you want to convert an address
to a contract, you need to convert `address` to `typed_address` using
`Test.Typed_address.to_typed_address`.

<Syntax syntax="cameligo">

Then cast `typed_address` to `unit contract` using
`Test.Typed_address.to_contract`. For example:

```cameligo test-ligo group=addr2contract
let test =
  let addr : address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" in
  let taddr : (unit, unit) typed_address = Test.Address.to_typed_address addr in
  let contract : unit contract = Test.Typed_address.to_contract taddr in
  contract
```

</Syntax>

<Syntax syntax="jsligo">

Then cast `typed_address` to `contract<unit>` using
`Test.Typed_address.to_contract`. For example:

```jsligo test-ligo group=addr2contract
const test = (() => {
  const addr : address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
  const taddr : typed_address<unit,unit> = Test.Address.to_typed_address(addr);
  const contract : contract<unit> = Test.Typed_address.to_contract(taddr);
  return contract;
})();
```

</Syntax>

Check out the reference of the `Test` framework for exact signature of the functions [here](../reference/test.md).
