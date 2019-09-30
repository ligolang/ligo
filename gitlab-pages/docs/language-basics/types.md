---
id: types
title: Types
---

## Built-in types

For the list of built-in types, please refer to the [Cheat Sheet](language-basics/cheat-sheet.md). LIGO's type system is built on top of Michelson, but offers a handful of features like type aliasing, or groupping of multiple types into a single powerful type.

## Type aliases

Type aliasing is a great choice when working towards a readable / maintainable smart contract. One well typed variable is worth a thousand words. For example we can choose to *alias* a string, as an animal breed - this will allow us to comunicate our intent with added clarity.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type animalBreed is string;

const dogBreed: animalBreed = "Saluki"; 
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Defining custom types

### Simple types
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
// accountBalances is a simple type, a map of address <-> tez
type accountBalances is map(address, tez);

const ledger: accountBalances = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> 10mtz
end
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Composed types

Often our contracts will require complex data structures, which will in turn require a well-typed storage, or functions to work with. LIGO offers a simple way to compose simple types, into larger & more expressive composed types.

In the example below you can see definition of data types for a ledger, that keeps a balance & number of previous transactions for a given account.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
// alias two types
type account is address;
type numberOfTransactions is nat;

// accountData consists of a record with two fields (balance, numberOfTransactions)
type accountData is record
    balance: tez;
    numberOfTransactions: numberOfTransactions;
end
// our ledger / accountBalances is a map of account <-> accountData
type accountBalances is map(account, accountData);

// pseudo-JSON representation of our map 
// { "tz1...": {balance: 10mtz, numberOfTransactions: 5n} }
const ledger: accountBalances = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> record
      balance = 10mtz;
      numberOfTransactions = 5n;
    end
end
```

<!--END_DOCUSAURUS_CODE_TABS-->