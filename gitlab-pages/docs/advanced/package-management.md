---
id: package-management
title: Package management
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';


Any programming language that aims to make collaboration easier needs
a way to distribute (and consume) its reusable modules. LIGO provides
first-class support for such distributable units (i.e. packages).

## Packages

Reusable modules that developers intend to share with others can be
distributed as packages by placing a `ligo.json` (a manifest file)
next to their Ligo modules.

```bash
$ ls
```

<br/>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">ligo.json</td>
<td class="org-left">set.mligo</td>
<td class="org-left">list.mligo</td>
</tr>
</tbody>
</table>

Any directory (recursively) containing `.mligo` files can be turned into a package
by simply placing a manifest file, `ligo.json` over there.

## LIGO registry

The [LIGO registry](https://packages.ligolang.org/) is used to host LIGO packages. The LIGO registry contains the contracts/libraries along with their metadata. The packages which reside on the LIGO registry can be installed using the `ligo install` command.

## Consuming

To fetch (download) & maintain different versions of external libraries we need a package manager.
LIGO libraries can be published to [LIGO's registry](https://packages.ligolang.org/) as well as [npm](https://www.npmjs.com/).
Using `ligo install` command we can fetch these ligo libraries.

Note:
Earlier versions of LIGO used [`esy`](https://esy.sh) as the backend for package management. This is not so anymore, and installing esy is not necessary.

### Workflow

We will need the LIGO compiler to compile smart contracts, to get the LIGO compiler follow these [instructions](https://ligolang.org/docs/intro/installation).

Next, we will use a simple dependency `@ligo/math-lib` published on the LIGO registry. To download & install the library, run,

```bash
$ ligo install @ligo/math-lib
```
<br/>

Now we can write a smart contract which will use the `@ligo/mathlib` library.

<Syntax syntax="cameligo">

```cameligo skip
#import "@ligo/mathlib/rational/rational.mligo" "Rational"

...

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
#import "@ligo/mathlib/rational/rational.mligo" "Rational"

...

```

</Syntax>

<br/>

> Note: When using LIGO packages via `#import`/`#include`
>
> If only the name of the package is provided, it will be resolved to
> the `main` file of the package.
>
> If you want to import a specific file from the package, the syntax is of the form
>
> `#import "<pkg name>/<file in package>" "Module"`
>
> `#include "pkg name>/<file in package>"`

and we write some tests for our smart contract in `main.test.mligo`

<!-- TODO: do these tests still work after the deprecation of main? -->

<Syntax syntax="cameligo">

```cameligo skip
#include "main.mligo"

let test =
    let storage = Test.compile_value [1; 2; 3] in
    let (addr, _, _) = Test.originate_from_file "./main.mligo" "main" ([] : string list) storage 0tez in
    let taddr : (parameter, storage) typed_address = Test.cast_address addr in
    let contr : parameter contract = Test.to_contract taddr in
    let _ = Test.transfer_to_contract_exn contr Reverse 1mutez in
    assert (Test.get_storage taddr = [3; 2; 1])
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
#include "main.jsligo"

const test = (() => {
    let storage = Test.compile_value(list([1, 2, 3]));
    let [addr, _, _] = Test.originate_from_file("./main.jsligo",
    "main", (list([]) as list<string>), storage, 0tez);
    let taddr : typed_address<parameter, storage> = Test.cast_address(addr);
    let contr : contract<parameter> = Test.to_contract(taddr);
    Test.transfer_to_contract_exn(contr, Reverse(), 1mutez);
    assert (Test.get_storage(taddr) == list([3, 2, 1]))
})();

```

</Syntax>

To compile the contract to Michelson run the command

<Syntax syntax="cameligo">

```bash
$ ligo compile contract main.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```bash
$ ligo compile contract main.jsligo
```

</Syntax>

This will find the dependencies installed on the local machine, and compile the `main.mligo` file.

To test the contract using LIGO's [testing framework](../advanced/testing.md) run the command

<Syntax syntax="cameligo">

```bash
$ ligo run test main.test.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```bash
$ ligo run test main.test.jsligo
```

</Syntax>

If you working with an existing LIGO project, to install the dependencies, at the root of the project just run

```bash
$ ligo install
```

### Specifying package versions

To use package, it is necessary to specify the exact version of the package. Semver ranges are currently not supported. This will, however, change soon.

### Upgrading the version of a LIGO package

During the lifecycle of a project, if you wish to upgrade the version of a LIGO package,
Just update the package version to the desired one in the `ligo.json`. e.g.

```diff
 {
   ...
   "dependencies": {
     "ligo-foo": "1.0.6",
-    "@ligo/bigarray": "1.0.0",
+    "@ligo/bigarray": "1.0.1",
     "ligo-test_2": "1.0.0",
     "ligo_test_1": "1.0.0"
   }
 }
```
<br/>

and run the command
```bash
$ ligo install
```
<br/>

This will fetch the updated version of the LIGO package, and the compiler will use the updated version of the package.

### Using a LIGO package via REPL

If you wish to try out a LIGO package in the REPL environment, Install
the LIGO package by following the steps above, and then fire up the
LIGO REPL using the following command

<Syntax syntax="cameligo">

```
$ ligo repl cameligo
Welcome to LIGO's interpreter!
Included directives:
  #use "file_path";;
  #import "file_path" "module_name";;
In  [1]: #import "@ligo/bigarray/lib/bigarray.mligo" "BA";;
Out [1]: Done.
In  [2]: BA.concat [1;2;3] [4;5;6];;
Out [2]: CONS(1 , CONS(2 , CONS(3 , CONS(4 , CONS(5 , CONS(6 , LIST_EMPTY()))))))
In  [3]:
```

</Syntax>

<Syntax syntax="jsligo">

```
$ ~/projects/ligo/_build/install/default/bin/ligo repl jsligo
Welcome to LIGO's interpreter!
Included directives:
  #use "file_path";;
  #import "file_path" "module_name";;
In  [1]: #import "@ligo/bigarray/lib/bigarray.mligo" "BA";;
Out [1]: Done.
In  [2]: BA.concat (list([1, 2, 3]))(list([4, 5, 6]));;
Out [2]: CONS(1 , CONS(2 , CONS(3 , CONS(4 , CONS(5 , CONS(6 , LIST_EMPTY()))))))
In  [3]:
```

</Syntax>

## Packaging

Packages are code units that can be shared with other developers. Therefore,
authors must provide useful metadata, both for other programmers in
the community as well as the LIGO toolchain, to understand the package's
contents, its version, and other useful information.

### Adding package metadata (LIGO manifest)

This is an important step, as it will help the tools and your users/collaborators, provide vital information about your package.

For LIGO packages, authors must provide a manifest file (ligo.json).

The structure of a LIGO manifest is as follows,

#### Required fields:

- **`name`** : Name of the package.
- **`version`** : Version of the package (Should be a valid [sem-ver](https://semver.org/)).
- **`main`** : The main file of the package, Ideally this file should export all the functionality that the package provides.
- **`author`** : Author of the package.
- **`license`** : A valid SPDX license identifier.
- **`repository`** : The place where the LIGO code is hosted (remote repository),
The `repository` field follows a [structure same as npm](https://docs.npmjs.com/cli/v9/configuring-npm/package-json#repository).
- **`bugs`** : The url to your project's issue tracker and/or the email address to which issues should be reported.
The `bugs` fields follows a [structure same as npm](https://docs.npmjs.com/cli/v9/configuring-npm/package-json#bugs).

- **`type`** : The `type` field can be one of `library` or `contract`, If the field is ommited default value of `type` is `library`
- **`storage_fn`** : In the case when `type` is `contract`, the name of the function which provides initial storage needs to be provided.
- **`storage_arg`** : In the case when `type` is `contract`, an expression that is a parameter to the `storage_fn` needs to be provided.

#### Optional fields:

- **`description`** : A brief description of the package.
- **`readme`** : Some readme text, if this field is omitted the contents of README.md or README will be used in its place.
- **`dependencies`** : A object (key-value pairs) of dependencies of the package where key is a `package_name` and the value is a `package_version`
- **`dev_dependencies`** : A object (key-value pairs) of dev_dependencies of the package where key is a `package_name` and value is a `package_version`


Sample LIGO manifest (`ligo.json`) with some of the above information:

```json
{
  "name": "math-lib",
  "version": "1.0.3",
  "description": "A math library for LIGO with support for Float & Rational numbers",
  "main": "lib.mligo",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/ligolang/math-lib-cameligo.git"
  },
  "author": "ligoLANG <https://ligolang.org/>",
  "license": "MIT",
  "bugs": {
    "url": "https://github.com/ligolang/math-lib-cameligo/issues"
  },
  "dependencies": {
    "math-lib-core": "^1.0.1",
    "math-lib-float": "^1.0.2",
    "math-lib-rational": "^1.0.1"
  }
}
```
<br/>

#### Ignore some files or directories while packaging using .ligoignore

You can specify some files or directories which you want to keep out of the LIGO package (keys, deployment scripts, etc.) in a `.ligoignore` file.
`.ligoignore` file is similar to a `.gitignore` file (you can specify glob patterns of files or directories you would like to ignore)

### Creating and publishing packages to the LIGO registry

We are going the write a simple package `ligo-list-helpers` library that is similar to the bigarray package we used earlier.


<Syntax syntax="cameligo">

```cameligo group=pkg
(* LIGO library for working with lists *)

let concat (type a) (xs : a list) (ys : a list) : a list =
    let f (x,ys : (a * a list)) : a list = x :: ys in
    List.fold_right f xs ys

let reverse (type a) (xs : a list) : a list =
    let f (ys,x : (a list * a)) : a list = x :: ys in
    List.fold_left f ([] : a list) xs

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=pkg
/* LIGO library for working with lists */

export const concat = <T>(xs : list<T>, ys : list<T>) : list<T> => {
    let f = ([x, ys] : [T, list<T>]) : list<T> => list([x, ...ys]);
    return List.fold_right(f, xs, ys)
}

export const reverse = <T>(xs : list<T>) : list<T> => {
    let f = ([ys, x] : [list<T>, T]) : list<T> => list([x, ...ys]);
    return List.fold_left(f, (list([]) as list<T>), xs)
}

```

</Syntax>
<br/>

and some tests for the library

<Syntax syntax="cameligo">

```cameligo skip
#include "list.mligo"

let test_concat =
    let xs = [1; 2; 3] in
    let ys = [4; 5; 6] in
    let zs = concat xs ys in
    assert (zs = [1; 2; 3; 4; 5; 6])

let test_reverse =
    let xs = [1; 2; 3] in
    assert (reverse xs = [3; 2; 1])

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
#include "list.jsligo"

const test_concat = (() => {
    let xs = list([1, 2, 3]);
    let ys = list([4, 5, 6]);
    let zs = concat(xs, ys);
    assert (zs == list([1, 2, 3, 4, 5, 6]))
})();

const test_reverse = (() => {
    let xs = list([1, 2, 3]);
    assert (reverse(xs) == list([3, 2, 1]))
})();

```

</Syntax>
<br/>

To run the tests run the command

<Syntax syntax="cameligo">

```bash
$ ligo run test list.test.mligo
```

</Syntax>
<Syntax syntax="jsligo">

```bash
$ ligo run test list.test.jsligo
```

</Syntax>

### Logging in

Before publishing, the registry server needs to authenticate the user to avoid abuse. To login,

```bash
$ ligo login
```
<br/>

If you're a new user,

```bash
$ ligo add-user
```
<br/>

This would create a `.ligorc` in the home directory.

> Note: By default, LIGO  creates the rc file (`.ligorc`) in the home directory.

### Publishing

LIGO packages can be published to a central repository at
[`packages.ligolang.org`](https://packages.ligolang.org/) with the `ligo publish` command.

```bash
$ ligo publish
==> Reading manifest... Done
==> Validating manifest file... Done
==> Finding project root... Done
==> Packing tarball... Done
    publishing: ligo-list-helpers@1.0.0
    === Tarball Details ===
    name:          ligo-list-helpers
    version:       1.0.0
    filename:      ligo-list-helpers-1.0.0.tgz
    package size:  895 B
    unpacked size: 1.1 kB
    shasum:        37737db2f58b572f560bd2c45b38e6d01277395d
    integrity:     sha512-a904c5af793e6[...]fc0efee74cfbb26
    total files:   6
==> Checking auth token... Done
==> Uploading package... Done
Package successfully published
```
<br/>

> Note: while publishing a package If just want to see what LIGO publish would do, you can use the `--dry-run` flag
> ```bash
> $ ligo publish --dry-run
> ```

## Quick CLI options reference

### --cache-path

By default dependencies are installed in the `.ligo` directory at the root of the project, If you wish to change
the path where dependencies are installed use the `--cache-path` option to specify the path e.g.

```bash
$ ligo install --cache-path PATH
```

### --project-root

LIGO will try to infer the root directory of the project so that it
can find the dependencies installed on your local machine, If you wish
to specify the root directory manually you can do so using the
`--project-root` option e.g.

<Syntax syntax="cameligo">

```bash
$ ligo compile contract main.mligo --project-root PATH
```

</Syntax>

<Syntax syntax="jsligo">

```bash
$ ligo compile contract main.jsligo --project-root PATH
```

</Syntax>

### --ligorc-path

LIGO creates a `.ligorc` file to store auth tokens for the user for a specific registry, This auth token is useful when publishing a package.

By default LIGO creates the `.ligorc` in the home directory, If you wish to override this you can do so using the `--ligorc-path` e.g.

```bash
# Loging in
$ ligo login --ligorc-path ./.ligorc

# Publishing
$ ligo publish --ligorc-path ./.ligorc
```
<br/>

> Note: Using `ligo login` users can log into multiple registries e.g. LIGO registry, and the LIGO beta registry, A new entry will be created in the `.ligorc` for storing auth token of each registry.

### --dry-run

While using `ligo publish` if you don't want to make changes to the LIGO registry, you can use the `--dry-run` flag. e.g.

```bash
$ ligo publish --dry-run
```
<br/>

This will only display the report on the command line what it would have done in the case of `ligo publish`.

## Unpublishing Packages

Packages can be published in two ways,

1. Completely delete the entry
2. Only unpublish a specific version

`unpublish` is that subcommand, grouped under `registry` subcommand, removes a package or a specific version of it from the registry

Summary
```
  [--package-name Name]      . of the package on which publish/unpublish is
                               executed
  [--package-version Version]
                             . of the package on which publish/unpublish is
                               executed
```

To unpublish a package, run `ligo registry unpublish --package-name <name> --package-version <version>` from anywhere on the CLI (not necessarily from within a project)

Examples,

```
ligo registry unpublish --package-name foo --package-version 1.0.0
```

If `--package-version` is skipped, the entire package is unpublished.



## Notes

### 1. Are packages written in different syntaxes interoperable?

Yes, any syntax can be used in packages. Furthermore, one can consume a package written in one syntax from another.

### 2. What happens if there are entry points declared in a LIGO package?

If you need to use the entry points defined within a package, the best approach is likely to alias them:

```cameligo skip
#import "package_name/increment.mligo" "Increment"

[@entry] let add = Increment.add
```
<br/>

In this case, only `add` entry point from the package will be used by the compiler. By adding an alias for `Increment.sub`, it is possible to also include that entry point in the final contract.

<!-- TODO: please review this section, I'm not 100% sure the entry points won't be automatically exported to the outer module -- Suzanne Soy 2023-09-18 -->

<!-- updated use of entry -->
