---
id: math-numbers-tez
title: Math, Numbers & Tez
---

LIGO offers three built-in numerical types: `int`, `nat` and `tez`.

## Addition

Addition in ligo is acomplished by using the `+` operator. Some type constraints apply; for example you can't add `tez + nat`.

In the following example you can find a series of arithmetic operations, including various numerical types. However, some bits of the example won't compile because adding an `int` to a `nat` produces an `int`, not a `nat`. Similiar rules apply for `tez`:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
// int + int produces int
const a: int = 5 + 10;
// nat + int produces int
const b: int = 5n + 10;
// tez + tez produces tez
const c: tez = 5mutez + 10mutez;
// you can't add tez + int or tez + nat, this won't compile
// const d: tez = 5mutez + 10n;
// two nats produce a nat
const e: nat = 5n + 10n;
// nat + int produces an int, this won't compile
// const f: nat = 5n + 10;
const g: int = 1_000_000;
```

> Pro tip: you can use underscores for readability when defining large numbers
>
>```pascaligo
>const g: int = 1_000_000;
>```

<!--CameLIGO-->

```cameligo
// int + int produces int
let a: int = 5 + 10
// nat + int produces int
let b: int = 5n + 10
// tez + tez produces tez
let c: tez = 5mutez + 10mutez
// you can't add tez + int or tez + nat, this won't compile
// const d: tez = 5mutez + 10n
// two nats produce a nat
let e: nat = 5n + 10n
// nat + int produces an int, this won't compile
// const f: nat = 5n + 10
let g: int = 1_000_000
```

> Pro tip: you can use underscores for readability when defining large numbers
>
>```cameligo
>let g: int = 1_000_000;
>```

<!--ReasonLIGO-->

```reasonligo
/* int + int produces int */
let a: int = 5 + 10;
/* nat + int produces int */
let b: int = 5n + 10;
/* tez + tez produces tez */
let c: tez = 5mutez + 10mutez;
/* you can't add tez + int or tez + nat, this won't compile */
/* let d: tez = 5mutez + 10n; */
/* two nats produce a nat */
let e: nat = 5n + 10n;
/* nat + int produces an int, this won't compile */
/* let f: nat = 5n + 10; */
let g: int = 1_000_000;
```

> Pro tip: you can use underscores for readability when defining large numbers
>
>```reasonligo
>let g: int = 1_000_000;
>```

<!--END_DOCUSAURUS_CODE_TABS-->

## Subtraction

The simpliest substraction looks like this:

> ⚠️ Even when subtracting two `nats`, the result is an `int`

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: int = 5 - 10;
// substraction of two nats, yields an int
const b: int = 5n - 2n;
// won't compile, result is an int, not a nat
// const c: nat = 5n - 2n;
const d: tez = 5mutez - 1mt;
```

<!--CameLIGO-->
```cameligo
let a: int = 5 - 10
// substraction of two nats, yields an int
let b: int = 5n - 2n
// won't compile, result is an int, not a nat
// const c: nat = 5n - 2n
let d: tez = 5mutez - 1mt
```

<!--ReasonLIGO-->
```reasonligo
let a: int = 5 - 10;
/* substraction of two nats, yields an int */
let b: int = 5n - 2n;
/* won't compile, result is an int, not a nat */
/* let c: nat = 5n - 2n; */
let d: tez = 5mutez - 1mt;
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Multiplication

You can multiply values of the same type, such as:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
const a: int = 5 * 5;
const b: nat = 5n * 5n;
// you can also multiply `nat` and `tez`
const c: tez = 5n * 5mutez;
```

<!--CameLIGO-->
```cameligo
let a: int = 5 * 5
let b: nat = 5n * 5n
// you can also multiply `nat` and `tez`
let c: tez = 5n * 5mutez
```

<!--ReasonLIGO-->
```reasonligo
let a: int = 5 * 5;
let b: nat = 5n * 5n;
/* you can also multiply `nat` and `tez` */
let c: tez = 5n * 5mutez;
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Division

In LIGO you can divide `int`, `nat`, and `tez`. Here's how:

> ⚠️ Division of two `tez` values results into a `nat`

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: int = 10 / 3;
const b: nat = 10n / 3n;
const c: nat = 10mutez / 3mutez;
```

<!--CameLIGO-->
```cameligo
let a: int = 10 / 3
let b: nat = 10n / 3n
let c: nat = 10mutez / 3mutez
```

<!--ReasonLIGO-->
```reasonligo
let a: int = 10 / 3;
let b: nat = 10n / 3n;
let c: nat = 10mutez / 3mutez;
```

<!--END_DOCUSAURUS_CODE_TABS-->

## From `int` to `nat` and back

You can *cast* an `int` to a `nat` and vice versa, here's how:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const a: int = int(1n);
const b: nat = abs(1);
```

<!--ReasonLIGO-->
```reasonligo
let a: int = int(1n);
let b: nat = abs(1);
```

<!--END_DOCUSAURUS_CODE_TABS-->