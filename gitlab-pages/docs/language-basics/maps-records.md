---
id: maps-records
title: Maps, Records
---

So far we've seen pretty basic data types. LIGO also offers more complex built-in constructs, such as Maps and Records.

## Maps

Maps are natively available in Michelson, and LIGO builds on top of them. A requirement for a Map is that its keys be of the same type, and that type must be comparable.

Here's how a custom map type is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type ledger is map(address, tez);
```

<!--Cameligo-->
```cameligo
type ledger = (address, tez) map
```

<!--END_DOCUSAURUS_CODE_TABS-->

And here's how a map value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
const ledger: ledger = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> 1000mtz;
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) -> 2000mtz;
end
```
> Notice the `->` between the key and its value and `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--Cameligo-->

```cameligo
let ledger: ledger = Map.literal
  [ (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address), 1000mtz) ;
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), 2000mtz) ;
  ]
```
> Map.literal constructs the map from a list of key-value pair tuples, `(<key>, <value>)`.
> Note also the `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.
<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing map values by key

If we want to access a balance from our ledger above, we can use the `[]` operator/accessor to read the associated `tez` value. However, the value we'll get will be wrapped as an optional; in our case `option(tez)`. Here's an example:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: option(tez) = ledger[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)];
```

<!--Cameligo-->

```cameligo
let balance: tez option = Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) ledger
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### Obtaining a map value forcefully

Accessing a value in a map yields an option, however you can also get the value directly:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: tez = get_force(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), ledger);
```

<!--Cameligo-->

```cameligo
let balance: tez = Map.find ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) ledger
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Records

Records are a construct introduced in LIGO, and are not natively available in Michelson. The LIGO compiler translates records into Michelson `Pairs`.

Here's how a custom record type is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type user is record 
    id: nat;
    is_admin: bool;
    name: string;
end
```

<!--Cameligo-->
```cameligo
type user = {
  id: nat;
  is_admin: bool;
  name: string;
}
```

<!--END_DOCUSAURUS_CODE_TABS-->

And here's how a record value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const user: user = record
    id = 1n;
    is_admin = True;
    name = "Alice";
end
```

<!--Cameligo-->
```cameligo
let user: user = {
  id = 1n;
  is_admin = true;
  name = "Alice";
}
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Accessing record keys by name

If we want to obtain a value from a record for a given key, we can do the following:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const is_admin: bool = user.is_admin;
```

<!--Cameligo-->
```cameligo
let is_admin: bool = user.is_admin
```

<!--END_DOCUSAURUS_CODE_TABS-->
