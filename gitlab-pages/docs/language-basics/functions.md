---
id: functions
title: Functions
---

## Defining a function

Body of a function consists of two parts, the first part (**`block {}`** or **`begin ... end`**) - normally consists of logic *(flow conditions, variable declarations, etc.)*, and the second part (**`with ...`**) usually defines the return value of your function.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
const availableSupply: nat = 15n;
const totalSupply: nat = 100n;

function calculatePrice(const available: nat; const total: nat): nat is
    begin
        const price: nat = total / available
    end with price

const price: nat = calculatePrice(availableSupply, totalSupply);
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Functions without an explicit body (shorter syntax)

A short hand syntax for the same function as above can inline the price calculation directly into the return statement.
While this approach can have it's benefits, it can decrease readability.
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
const availableSupply: nat = 15n;
const totalSupply: nat = 100n;

function calculatePrice(const available: nat; const total: nat): nat is
    block { skip } with total / available

const price: nat = calculatePrice(availableSupply, totalSupply);
```

<!--END_DOCUSAURUS_CODE_TABS-->