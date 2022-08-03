---
id: package-management
title: Package management
---

# Table of Contents

1.  [Ligo registry](#org1abc41f)
    1.  [Packages](#org9972a92)
    2.  [Packaging](#org1f8c72e)
    3.  [Publishing](#orgf4e121c)
    4.  [Consuming](#orgd2a9667)
    5.  [Notes](#org1cc9c85)


<a id="org1abc41f"></a>

# Ligo registry

Any programming language that aims to make collaboration easier needs
a way to distribute (and consume) it's reusable modules. Ligo provides
first-class support for such distributable units (ie. packages).


<a id="org9972a92"></a>

## Packages

Reusable modules that developers intend to share with other can be
distributed as packages by placing a `package.json` (a manifest file)
next to their Ligo modules.

    ls

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">package.json</td>
<td class="org-left">set.mligo</td>
<td class="org-left">list.mligo</td>
</tr>
</tbody>
</table>

Any directory (recursively) containing `.mligo` files can be turned into a package
by simply placing a manifest file, `package.json` over there.


<a id="org1f8c72e"></a>

## Packaging

Packages are code units shared with other developers. Therefore,
authors must provide useful metadata, both for other programmers in
the community as well as ligo toolchain, to understand the package's
contents, it's version and other useful information.

For Ligo packages, authors must provide,

1.  A name (in the `name` field)
2.  Version (`version`)
3.  Dependencies, if any. (`dependencies`)
4.  A brief description (`description`)

Sample `package.json` with the above information.

    {
      "name": "ligo-foo",
      "version": "1.0.18",
      "name": "Name <email@domain.com>"
      "description": "An example for ligo dependency depending on another ligo dependency",
      "scripts": {
        "test": "ligo run test foo.test.mligo --project-root ."
      },
      "dependencies": {
        "ligo-list-helpers": "1.0.0",
        "ligo-set-helpers": "^1.0.2"
      }
    }


<a id="orgf4e121c"></a>

## Publishing

Ligo packages can be published to a central repository at
`packages.ligolang.org` with the `ligo publish` command.

    ligo publish


<a id="orgd2a9667"></a>

## Consuming

To fetch (download) & maintain different versions of external libraries we need a package manager.
LIGO libraries can be published to [Ligo's own registry](https://packages.ligolang.org) as well as [npm](https://www.npmjs.com/).
Using `ligo install` command we can fetch these ligo libraries (It internally invokes the [esy](https://esy.sh/) package manager).

Pre-requites: 
1. esy [link](https://esy.sh/docs/en/getting-started.html)

## Workflow

Start with empty `package.json` file

```json
{}
```

We will need the LIGO compiler binary to compile smart contracts, to get the LIGO compiler follow these [instructions](https://ligolang.org/docs/intro/installation).

Next we will use a simple dependency `ligo-list-helper` published on Ligo registry. To download & install the library, run,

```bash
ligo install ligo-list-helpers
```

Now we will write a smart contract `main.mligo` which will use the `ligo-list-herpers` library

```cameligo skip
#import "ligo-list-helpers/list.mligo" "XList"

type parameter =
  Concat  of int list
| Reverse

type storage = int list

type return = (operation) list * storage

let main (action, store : parameter * storage) : operation list * storage =
  (([]: operation list),
   (match action with
      Concat ys -> XList.concat store ys 
    | Reverse   -> XList.reverse store))

```

and we write some tests for our smart contract in `main.test.mligo`

```cameligo skip
#include "main.mligo"

let test = 
    let storage = Test.compile_value [1; 2; 3] in
    let (addr, _, _) = Test.originate_from_file "./main.mligo" "main" ([] : string list)) storage 0tez in
    let taddr : (parameter, storage) typed_address = Test.cast_address addr in
    let contr : parameter contract = Test.to_contract taddr in
    let _ = Test.transfer_to_contract_exn contr Reverse 1mutez in
    assert (Test.get_storage taddr = [3; 2; 1])

```

To compile the contract to Michelson run the command

```bash
ligo compile contract main.mligo
```

This will find the dependencies installed on the local machine, and compile the `main.mligo` file.

To test the contract using ligo's [testing framework](https://ligolang.org/docs/reference/test) run the command

```bash
ligo run test main.test.mligo
```

If you working with an existing LIGO project, to install the dependencies, at the root of the project just run

```bash
ligo install
```

### Upgrading version of a LIGO package

During the lifecycle of a project, if you wish to upgrade the version of a LIGO package, 
Just update the package version to the desired one in the `package.json`.

```diff
{
  ...
  "dependencies": {
    "ligo-foo": "1.0.6",
-   "ligo-list-helpers": "1.0.0",
+   "ligo-list-helpers": "1.0.1",
    "ligo-test_2": "1.0.0",
    "ligo_test_1": "1.0.0"
  }
}
```
and run the command
```bash
ligo install
```
This will fetch the updated version of LIGO package, and the compiler will use the updated
version of the package.

### Using a LIGO package via REPL

If you wish to try out a LIGO package in the REPL environment, Install the LIGO package by
following the steps above,

And then fire up the LIGO REPL using the following command

```
$ ligo repl cameligo
Welcome to LIGO's interpreter!
Included directives:
  #use "file_path";;
  #import "file_path" "module_name";;
In  [1]: #import "ligo-list-helpers/list.mligo" "ListX";;
Out [1]: Done.
In  [2]: ListX.concat;;
Out [2]: "[lambda of type: (lambda (list int) (lambda (list int) (list int))) ]"
In  [3]: ListX.concat [1;2;3] [4;5;6];;
Out [3]: CONS(1 , CONS(2 , CONS(3 , CONS(4 , CONS(5 , CONS(6 , LIST_EMPTY()))))))
In  [4]: 
```

## Creating and publishing packages to Ligo registry

We are going the write the `ligo-list-helpers` library that we used earlier.

```cameligo skip
(* LIGO library for working with lists *)

let concat (type a) (xs : a list) (ys : a list) : a list =
    let f (x,ys : (a * a list)) : a list = x :: ys in
    List.fold_right f xs ys

let reverse (type a) (xs : a list) : a list =
    let f (ys,x : (a list * a)) : a list = x :: ys in
    List.fold_left f ([] : a list) xs

```

and some tests for the library

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

To run the tests run the command

```bash
ligo run test list.test.mligo
```

#### Adding package metadata

This is an important step, as it will help the tools and your users/collaborators, provide vital information about your package.

```json
{
      "name": "ligo-foo",
      "version": "1.0.18",
      "description": "An example for ligo dependency depending on another ligo dependency",
      "dependencies": {
        "ligo-list-helpers": "1.0.0",
        "ligo-set-helpers": "^1.0.2"
      }
```

#### Logging in

Before publishing, registry server needs to authenticate the user to avoid abuse. To login,

```bash
ligo login
```

If you're a new user,

```bash
ligo add-user
```
This would create a `.ligorc` in the home directory.

> Note: unlike npm, ligo only creates the rc file in the home directory. Placing the file anywhere else isn't meaningful to Ligo

Now run,

```bash 
ligo publish
```

<a id="org1cc9c85"></a>

## Quick CLI options reference

### --cache-path

By default dependencies are installed in the `.ligo` directory at the root of the project, If you wish to change
the path where dependencies are installed use the `--cache-path` option to specify the path e.g.

```bash
ligo install --cache-path PATH
```

### --project-root

LIGO will try to infer the root directory of the project so that it can find the dependencies installed on your local machine, 
If you wish to specify the root directory manually you can do so using the `--project-root` option e.g.

```bash
ligo compile contract main.mligo --project-root PATH
```

## Notes

Note that,

1.  References made to cameligo are only for illustrative purposes. Any
    syntax can be used in packages. Furthermore, one can consume a
    package written in one syntax from another.

2.  What happen if there is a main function in a .mligo file?
    
    Depends on how it is called.

If it is not used, it won't appear in the final michelson - only the used parts from the library will be compiled.

As an example, consider,

    #import "package_name/increment.mligo" "Increment"
    
    let main =
      ...
      Increment.add ...
      ...

In this case, only add function from the package will be used by the compiler.

Also,

    #import "package_name/increment.mligo" 
    
    let test = 
      Test.originate ... Increment.main ...

In this case main function will be used in tests.
