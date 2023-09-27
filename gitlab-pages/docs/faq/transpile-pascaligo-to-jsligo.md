---
id: transpile-pascaligo-to-jsligo
title: How to transpile a contract from PascaLIGO to JsLIGO ?
---

import Syntax from '@theme/Syntax';

For a given PascaLIGO contract, called `contract.ligo` for example.

### Step 1 : Compilation in the source syntax (PascaLIGO)

First, make sure that your PascaLIGO code compiles with the current version of the LIGO compiler :

```
ligo compile contract --deprecated contract.ligo
```

### Step 2 : Transpilation

Then, to transpile the contract to JsLIGO, if you want to save the transpiled contract to a file called `transpiled_contract.jsligo` for example, use :

```
ligo transpile contract contract.ligo -o transpiled_contract.jsligo
```

Or if you want to dump the transpiled contract to the standard output, you can use :

```
ligo transpile contract contract.ligo --to-syntax jsligo
```

_**Note :** The ligo compiler guesses the syntax of the source file thanks to its file extension (`.ligo` here),
but to be extra explicit, you can also use the `--from-syntax` to specify the syntax of the source file._

### Step 3 : Compilation in the target syntax (JsLIGO)

To compile the translated contract, use `ligo compile contract` as usual,
but don't forget to add the `--transpiled` flag to the command line.

The `--transpiled` flag will tell the LIGO compiler that the JsLIGO contract comes from transpilation,
and will thus disable certain checks that don't make sense with transpiled contracts.

```
ligo compile contract --transpiled foo.jsligo
```

<!-- updated use of entry -->