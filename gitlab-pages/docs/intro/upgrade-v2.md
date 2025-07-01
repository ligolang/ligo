---
title: Migrating to LIGO v2.0
---

import Syntax from '@theme/Syntax';

Version 2.0 of LIGO includes breaking changes, so you must update your code when you upgrade your installation of LIGO as described in [Installation](./installation).

<Syntax syntax="jsligo">

:::tip

LIGO 2.0 includes a migration tool that can make some of the necessary changes to JsLIGO code.
See [Using the upgrade tool](#using-the-upgrade-tool).

:::

</Syntax>

## Promotion of `Test.Next`

The `Test.Next` library is now the `Test` library, replacing the old `Test` library.
This table shows the old functions and their new equivalents:

| Function in LIGO v1.9.2 | Function in LIGO v2 |
| --- | --- |
| `Test.run`| `Test.Michelson.run`|
| `Test.eval`| `Test.Michelson.eval`|
| `Test.decompile`| `Test.Michelson.decompile`|
| `Test.compile_value`| `Test.Michelson.eval`|
| `Test.get_total_voting_power`| `Test.State.get_total_voting_power`|
| `Test.failwith`| `Test.Assert.failwith`|
| `Test.to_contract`| `Test.Typed_address.to_contract`|
| `Test.set_source`| `Test.State.set_source`|
| `Test.cast_address`| `Test.Address.to_typed_address`|
| `Test.to_address`| `Test.Typed_address.to_address`|
| `Test.get_storage`| `Test.Typed_address.get_storage`|
| `Test.get_storage_of_address`| `Test.Address.get_storage`|
| `Test.get_balance_of_address`| `Test.Address.get_balance`|
| `Test.get_balance`| `Test.Typed_address.get_balance`|
| `Test.print`| `Test.IO.print`|
| `Test.eprint`| `Test.IO.eprint`|
| `Test.get_voting_power`| `Test.State.get_voting_power`|
| `Test.nth_bootstrap_contract`| `Test.Account.Contract.bootstrap`|
| `Test.nth_bootstrap_account`| `Test.Account.address`|
| `Test.get_bootstrap_account`| `Test.Account.info`|
| `Test.nth_bootstrap_typed_address`| `Test.Account.Contract.bootstrap_typed_address`|
| `Test.last_originations`| `Test.State.last_originations`|
| `Test.random`| `Test.random` (unchanged) |
| `Test.new_account`| `Test.Account.new`|
| `Test.bake_until_n_cycle_end`| `Test.State.bake_until`|
| `Test.get_time`| `Tezos.get_now` |
| `Test.register_delegate`| `Test.State.register_delegate`|
| `Test.stake`| `Test.State.stake`|
| `Test.register_constant`| `Test.State.register_constant`|
| `Test.to_typed_address`| `Test.Contract.to_typed_address`|
| `Test.constant_to_michelson_program`| `Test.Michelson.parse`|
| `Test.parse_michelson`| `Test.Michelson.parse`|
| `Test.restore_context`| `Test.State.restore`|
| `Test.save_context`| `Test.State.save`|
| `Test.drop_context`| `Test.State.drop`|
| `Test.to_string`| `Test.String.show`|
| `Test.to_json`| `Test.String.json`|
| `Test.to_debugger_json`| `Test.String.debugger_json`|
| `Test.set_baker_policy`| `Test.State.set_baker_policy`|
| `Test.set_baker`| `Test.State.set_baker`|
| `Test.size`| `Test.Michelson.Contract.size`|
| `Test.compile_contract`| `Test.Michelson.Contract.compile`|
| `Test.read_contract_from_file`| `Test.Michelson.Contract.from_file`|
| `Test.chr`| `Test.String.chr`|
| `Test.nl`| `Test.String.nl`|
| `Test.println`| `Test.IO.println`|
| `Test.set_print_values`| `Test.IO.set_test_print`|
| `Test.unset_print_values`| `Test.IO.unset_test_print`|
| `Test.get_last_events_from`| `Test.State.last_events`|
| `Test.transfer`| `Test.Typed_address.transfer`|
| `Test.transfer_exn`| `Test.Typed_address.transfer_exn`|
| `Test.log`| `Test.IO.log`|
| `Test.reset_state`| `Test.State.reset`|
| `Test.reset_state_at`| `Test.State.reset_at`|
| `Test.bootstrap_contract`| `Test.State.Reset.add_func_contract`|
| `Test.mutate_value`| `Test.Mutation.value`|
| `Test.save_mutation`| `Test.Mutation.save`|
| `Test.sign`| `Test.Crypto.sign`|
| `Test.add_account`| `Test.Account.add`|
| `Test.baker_account`| `Test.State.Reset.add_baker`|
| `Test.set_big_map`| `Test.State.set_big_map`|
| `Test.transfer_to_contract`| `Test.Contract.transfer`|
| `Test.transfer_to_contract_exn`| `Test.Contract.transfer_exn`|
| `Test.michelson_equal`| `Test.Compare.eq`|
| `Test.to_entrypoint`| `Test.Typed_address.get_entrypoint`|
| `Test.storage_with_dynamic_entrypoints`| `Test.Dynamic_entrypoints.storage`|
| `Test.originate_contract`| `Test.Originate.michelson`|
| `Test.compile_contract_with_views`| `Test.Michelson.Contract.compile_with_views`|
| `Test.originate`| `Test.Originate.contract`|
| `Test.compile_contract_from_file`| `Test.Michelson.Contract.from_file`|
| `Test.originate_from_file`| `Test.Originate.from_file`|
| `Test.mutation_test`| `Test.Mutation.func`|
| `Test.mutation_test_all`| `Test.Mutation.All.func`|
| `Test.originate_from_file_and_mutate`| `Test.Mutation.from_file`|
| `Test.originate_from_file_and_mutate_all`| `Test.Mutation.All.from_file`|
| `Test.originate_module_and_mutate`| `Test.Mutation.contract`|
| `Test.originate_and_mutate_all`| `Test.Mutation.All.contract`|
| `Test.assert`| `Test.Assert.assert`|
| `Test.assert_some`| `Test.Assert.some`|
| `Test.assert_none`| `Test.Assert.none`|
| `Test.assert_with_error`| `Test.Assert.Error.assert`|
| `Test.assert_some_with_error`| `Test.Assert.Error.some`|
| `Test.assert_none_with_error`| `Test.Assert.Error.none`|
| `Test.equal`| `Test.Compare.eq`|
| `Test.not_equal`| `Test.Compare.neq`|
| `Test.greater`| `Test.Compare.gt`|
| `Test.less`| `Test.Compare.lt`|
| `Test.greater_or_equal`| `Test.Compare.ge`|
| `Test.less_or_equal`| `Test.Compare.le`|
| `Test.create_chest`| `Test.Timelock.create`|
| `Test.create_chest_key`| `Test.Timelock.create_key`|

<Syntax syntax="cameligo">

</Syntax>

<Syntax syntax="jsligo">

## Contract syntax

In previous versions of JsLIGO, contracts could be defined in namespaces or at the top level of a file.
Version 2.0 adds the option to define contracts in classes.
Whether you define contracts in namespaces, classes, or in the top level of a file depends on the features that you need and the syntax that you prefer.
For example, if you want a contract to implement an interface, you must put the contract in a class.

The syntax for contracts in namespaces and in the top levels of files has changed in some ways:

- As before, they can include type definitions
- As before, they must define entrypoints as functions with the `function`, `const`, or `let` properties
- They must put decorators such as `@entry` and `@view` in a comment immediately before the definition that they modify
- They cannot implement interfaces

Here is an example of a JsLIGO 2.0 contract in a namespace.
The only change for JsLIGO 2.0 is that the decorators are now in comments:

```jsligo group=namespace
namespace Counter {
  type storage_type = int;
  type return_type = [list<operation>, storage_type];

  // @entry
  const add = (value: int, storage: storage_type): return_type =>
    [[], storage + value];

  // @entry
  const sub = (value: int, storage: storage_type): return_type =>
    [[], storage - value];
}
```

The syntax for a JsLIGO 2.0 contract at the top level of a file is similar.
The only change for JsLIGO 2.0 is that the decorators are now in comments:

```jsligo group=toplevel
type storage_type = int;
type return_type = [list<operation>, storage_type];

// @entry
const add = (value: int, store: storage_type): return_type =>
  [[], store + value];

// @entry
const sub = (value: int, store: storage_type): return_type =>
  [[], store - value];
```

However, the syntax for a JsLIGO 2.0 contract in a class is different, as described in [Classes](#classes).

## Namespaces

As described in [Contract syntax](#contract-syntax), the syntax for namespaces (whether they define a contract or merely a group of other related definitions) has changed in some ways:

- As before, they can include type definitions
- As before, they must define entrypoints as functions with the `function`, `const`, or `let` properties
- They must put decorators such as `@entry` and `@view` in a comment immediately before the definition that they modify
- They cannot implement interfaces

## Classes

JsLIGO 2.0 introduces classes, which are similar to classes in JavaScript/Typescript.
You can use JsLIGO classes to define a smart contract or group related functions, which are referred to as properties when they are in a class.
However, JsLIGO classes do not have all of the same features as JavaScript/TypeScript classes.
For example, JsLIGO classes cannot inherit from other classes as they can in JavaScript/TypeScript.

JsLIGO classes have these characteristics:

- They cannot include type definitions
- They define functions (including entrypoints) without the `function`, `const`, or `let` keywords
- They do not need to put decorators such as `@entry` and `@view` in comments
- They can implement interfaces

Here is an example contract that is defined in a class:

```jsligo group=classes
type storage_type = int;
type return_type = [list<operation>, storage_type];

class Counter {
  @entry
  add = (value: int, storage: storage_type): return_type =>
    [[], storage + value];

  @entry
  sub = (value: int, storage: storage_type): return_type =>
    [[], storage - value];
}
```

To encapsulate classes with types, such as if you want to define a type that represents the contract storage or complex parameter types, you can group the contract class and the related types in a namespace.

## Interfaces

The syntax for interfaces has changed; they now have a syntax that is similar to classes:

- Like classes, they cannot contain type definitions.
- They contain function signatures that are not defined with the `function`, `const`, or `let` properties.
- Unlike classes, they must put decorators such as `@entry` and `@view` in a comment immediately before the definition that they modify.

For example, this is an interface in JsLIGO 1.9.2:

```jsligo skip
// Not valid in JsLIGO 2.0
interface MyInterface {
  type storage = int;
  @entry
  const add: (s: int, k: storage) => [list<operation>, storage];
}
```

This is the equivalent interface in JsLIGO 2.0:

```jsligo group=interfaces
type storage = int;
interface MyInterface {
  // @entry
  add: (s: int, k: storage) => [list<operation>, storage];
}
```

As a result of these changes, abstract types (type names that are required by an interface without specifying the actual type) are no longer permitted in JsLIGO.

## Types

JsLIGO 2.0 includes changes to make its type system more similar to TypeScript.

### Specifying the types of literals

Instead of using postfixes to denote certain types, LIGO 2.0 uses the `as` keyword to denote the type of a literal, as in these examples:

Data type | LIGO 1.0 | LIGO 2.0
--- | --- | ---
Bytes | `const zero = 0x` | `const zero = "" as bytes`
Natural numbers | `const one = 1n` | `const one = 1 as nat`
tez | `const one_tez = 1tez` | `const one_tez = 1 as tez`
mutez | `const one_mutez = 1mutez` | `const one_mutez = 1 as mutez`

Similarly, the `parameter_of` keyword now takes its type in a way more similar to TypeScript:

LIGO 1.0 | LIGO 2.0
--- | ---
`const parameter = MyEntrypoint(value) as parameter_of MyContract;` | `const parameter = MyEntrypoint(value) as parameter_of<MyContract>;`

### Records (now called objects)

To be closer to JavaScript, JslIGO now refers to the record data type as an object.
JsLIGO objects are similar to JavaScript objects, but they have some limitations that JavaScript objects don't have.

To specify the type of an object, use commas to separate the fields, not semicolons as in the previous version of JsLIGO.
Continue to use commas to separate the values in an object, as in the previous version of JsLIGO and as in JavaScript.
For example, this contract uses an object type in JsLIGO 2.0:

```jsligo group=objects
type object_type = {x: int, y: int, z: int};
type storage_type = object_type;
type return_type = [list<operation>, storage_type];

class Counter {
  @entry
  reset = (_: unit, _s: storage_type): return_type => {
    const newObject: object_type = {
      x: 0,
      y: 0,
      z: 0,
    };
    return [[], newObject];
  }

  @entry
  increment = (_: unit, storage: storage_type): return_type => {
    const { x, y, z } = storage;
    const newObject: object_type = {
      x: x + 1,
      y: y + 1,
      z: z + 1,
    };
    return [[], newObject];
  }
}
```

Field name punning (a shorthand way of setting the value of a field to a variable of the same name) is no longer permitted in object type definitions.
This example shows how to avoid punning by specifying the type of each field explicitly:

```jsligo group=type_punning
type x = int;
type y = int;
type z = int;
// type object_type = { x, y, z }; // not permitted
type object_type = {x: x, y: y, z: z}; // correct
```

Punning is still permitted in variable definitions, as in this example:

```jsligo group=value_punning
type object_type = { x: int, y: int, z: int };
const x = 5;
const y = 6;
const z = 7;
const newObject: object_type = { x, y, z };
```

JsLIGO objects have these limitations that JavaScript objects don't have:

- You cannot change the fields of a JsLIGO object that is declared as a constant.
However, you can change the fields of a JsLIGO object that is declared as a variable.
- You cannot add fields to an object after you create it, regardless of whether it is declared as a constant or a variable.
- You cannot access fields in an object by putting a variable name in brackets; you must access the fields by specifying the field names as literals in brackets or with the selection operator (`.`).

For more information about objects, see [Objects](../data-types/records).

### Variants and options

The syntax for variant and option types has changed.
If the variant type has only one variant, it must start with a vertical bar, as in this example:

```jsligo group=union
type singleton = | ["Single"];
```

To define the values of a variant type, use the usual tuple syntax, with the type as the first tuple value and the value of the variant as the second tuple value.
To prevent the compiler from seeing the first value as a string, you must set its type as one of the constructors from the variant type, as in this example:

```jsligo group=union
type user =
  ["Admin", nat]
| ["Manager", int]
| ["Guest"];

const admin1: user = ["Admin" as "Admin", 1 as nat];
const manager2: user = ["Manager" as "Manager", 2 as int];
const guest3: user = ["Guest" as "Guest"];
```

## Pattern matching

The syntax for pattern matching has changed to use the `$match` predefined function instead of the `match` keyword.
As its parameters, the `$match` function receives the variant value to match on and an object.
The property names of the object are the constructors of the corresponding variant type.
The property values are either a single expression or functions that receive the value of the variant as a parameter.
Here is an example:

```jsligo group=matching
type user =
  ["Admin", nat]
| ["Manager", nat]
| ["Guest"];

const greetUser = (user: user): string => {
  $match (user, {
    "Admin": _id => "Hello, administrator",
    "Manager": _id => "Welcome, manager",
    "Guest": () => "Hello, guest",
  });
}
```

To define the thunk, use an immediately invoked function expression (IIFE) as the result of a match, as in the following example.
JsLIGO 2.0 uses this syntax in place of `do` expressions in the previous version.

```jsligo group=matching
const getSquareOfUserId = (user: user): nat => {
  $match (user, {
    "Admin": id => (() => {
      const squareOfId = id * id;
      return squareOfId;
    })(),
    "Manager": id => (() => {
      const squareOfId = id * id;
      return squareOfId;
    })(),
    "Guest": () => 0,
  });
}
```

Matching works only on values of variant types.
If you want to use the `$match` statement on other types or on more than one variable at a time, you can wrap them in a variant or option.
This example wraps two types in a variant so the `$match` statement can use them both:

```jsligo group=matching
type wrapper = | ["Wrap", [int, int]];
const wrapped: wrapper = ["Wrap" as "Wrap", [5, 5]];

const compareResult: string = $match(wrapped, {
  "Wrap": ([_a, _b]) => (() => {
    if (_a == _b) return "Equal";
    if (_a > _b) return "Greater";
    if (_a < _b) return "Less";
    return "Default";
  })(),
});
```

Also, match cases can accept only one parameter.
In LIGO v1, the following match statement was allowed; note that the match expression for the `RGB` case accepts three parameters, one for each of the values in the variant:

```jsligo skip
type colour =
| ["RGB", [int, int, int]]
| ["Gray", int]
| ["Default"];

let colourInt: colour = RGB(1, 2, 3);
let result = match(colourInt) {
  when(Gray(val)): do {
    const a = 5n;
    const b = 6n;
    return a + b + abs(val);
  };
  when(RGB(a, b, c)): do {
    return abs(a + b + c);
  }
  when(Default): do {
    return 5n;
  };
}
```

The equivalent match expression in LIGO v2 accepts only one parameter, a tuple that contains the values from the variant case:

```jsligo group=match_case_tuple
type colour =
  ["RGB", [int, int, int]] | ["Gray", int] | ["Default"];

let colourInt: colour = ["RGB" as "RGB", 1, 2, 3];
let result =
  $match(colourInt, {
    "Gray": (val) =>
      (() =>
      {
        const a = (5 as nat);
        const b = (6 as nat);
        return a + b + abs(val);
      })(),
    "RGB": ([a, b, c]) =>
      (() =>
      { return abs(a + b + c); })(),
    "Default": () =>
      (() =>
      { return (5 as nat); })(),
  })
```

## Imports

JsLIGO now uses a syntax closer to JavaScript/TypeScript to import definitions.
It supports three syntaxes for importing definitions from other files and namespaces:

- `import M = M.O`
- `import * as M from "./targetFile.jsligo"`
- `import {x, y} from "./targetFile.jsligo"`

In most cases, to import definitions from other files, use the syntax `import * as M from "./targetFile.jsligo"` to import everything in a file and then create local definitions from that imported file.

For example, assume that this file is `myFunctions.jsligo`:

```jsligo group=myFunctions
export namespace MyFunctions {
  export const addToImport = (a: int, b: int): int => a + b;
  export const subToImport = (a: int, b: int): int => a - b;
}
```

You can import the file and access the namespace like this:

```jsligo group=useMyFunctions
import * as MyFileWithFunctions from './gitlab-pages/docs/intro/src/upgrade-v2/myFunctions.jsligo';
const addToImport = MyFileWithFunctions.MyFunctions.addToImport;
const subToImport = MyFileWithFunctions.MyFunctions.subToImport;

namespace Counter {
  type storage_type = int;
  type return_type = [list<operation>, storage_type];

  // @entry
  const add = (value: int, storage: storage_type): return_type =>
    [[], addToImport(storage, value)];

  // @entry
  const sub = (value: int, storage: storage_type): return_type =>
    [[], subToImport(storage, value)];

}
```

If variables or functions are defined at the top level of the file, you can import them directly with the syntax `import {x, y} from "./targetFile.jsligo"`.
You cannot import types, classes, or namespaces with this syntax.

For example, assume that this file is `topLevelDefinitions.jsligo`:

```jsligo group=topLevelDefinitions
export const addToImport = (a: int, b: int): int => a + b;
export const subToImport = (a: int, b: int): int => a - b;
export const myConstant = 5 as int;
```

You can import and use those definitions as in this example:

```jsligo group=useTopLevelDefinitions
import { addToImport, subToImport, myConstant } from "./gitlab-pages/docs/intro/src/upgrade-v2/topLevelDefinitions.jsligo";

type storage_type = int;
type return_type = [list<operation>, storage_type];

class Calculator {

  @entry
  add = (value: int, storage: storage_type): return_type =>
    [[], addToImport(storage, value)];

  @entry
  sub = (value: int, storage: storage_type): return_type =>
    [[], subToImport(storage, value)];

  @entry
  increment = (_: unit, storage: storage_type): return_type =>
    [[], addToImport(storage, myConstant)];

}
```

You cannot import types as in this TypeScript syntax:

```jsligo skip
// Not allowed
import { type x } from "./myTypes.jsligo";
```

Instead, import the file and bind a type locally.
For example, assume that this file is `myTypes.jsligo`:

```jsligo group=myTypes
export type storage_type = int;
export type return_type = [list<operation>, storage_type];
```

This file imports those types and uses them:

```jsligo group=use_myTypes
import * as myTypes from "./gitlab-pages/docs/intro/src/upgrade-v2/myTypes.jsligo";
type storage_type = myTypes.storage_type;
type return_type = myTypes.return_type;

class Counter {
  @entry
  add = (value: int, storage: storage_type): return_type =>
    [[], storage + value];

  @entry
  sub = (value: int, storage: storage_type): return_type =>
    [[], storage - value];
}
```

## Preprocessor directives

Preprocessor directives are no longer used in JsLIGO version 2.0.
Here are some ways to update code that uses preprocessor directives:

- Instead of using the `#include` or `#import` directives, import namespaces in other files directly with the `import` keyword as described previously.

- The `#if`, `#else`, `#elif`, `#endif`, `#define`, `#undef`, and `#error` directives are no longer supported.
If you need to continue using them, you can run your JsLIGO code through a C++ preprocessor, which uses the same syntax.
JsLIGO code with these directives does not compile.

## The `switch` statement

Blocks that use the `switch` statement must have at least one case.
Also, `switch` blocks can have only one default case.

## `do` expressions

Instead of the expression `do { ... }`, use an IIFE in the format `(() => {})()`.
IIFEs are often used to run tests, as in this example:

```jsligo skip
const test = (() => {

  const contract = Test.Originate.contract(contract_of(Counter), 0, 0 as tez);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("add", contract.taddr), 5, 0 as tez);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("sub", contract.taddr), 2, 0 as tez);
  Assert.assert(Test.Typed_address.get_storage(contract.taddr) == 3);

})();
```

## Escaping keywords

Escaping keywords is no longer supported; keywords cannot be used as variable names or object fields, even if you prefix the names with the `@` symbol.
For example, you can add an underscore as a suffix, creating variables with names such as `return_` or `entry_`.

## Decorators

Decorators such as `@entry` and `@view` must now be in a comment immediately before the definition that they modify except when they are in classes, as described above.

## Commands

The `ligo run dry-run` command accepts a different format for entrypoints and parameters.
Instead of accepting the name of the entrypoint, it accepts the entrypoint and parameter as a value of a variant type with the name of the entrypoint.
The name of the entrypoint is always capitalized.
For example, this command calls an entrypoint named `add` that accepts an integer as a parameter:

```bash
ligo run dry-run counter.jsligo -m "Counter" '["Add" as "Add", 1]' 5
```

## Using the upgrade tool

The upgrade tool makes some changes to JsLIGO files to upgrade them to version 2.0 but cannot make all of the necessary changes.

:::caution

After using the tool, you must manually verify that your code works as expected.

:::

Follow these steps to run the upgrade tool:

1. Verify that your code compiles and your tests pass with JsLIGO 1.x.

1. TODO how to build or install the tool.

1. Run the tool on your files by running this command, where the variable `<JSLIGO_FILE>` is the file to convert:

   ```bash
   TODO_COMMANDNAME --upgrade --namespaces -- <JSLIGO_FILE>
   ```

   In most cases, include the `--namespaces` switch to convert namespaces to classes.
   For more information, see [Converting namespaces to classes](#converting-namespaces-to-classes).

   :::tip

   Run the upgrade tool on files that are imported before running it on files that import those files.

   :::

1. (Optional): Run a linter or formatter on the code to update its style to meet your preferences.

1. Make the manual changes described in [Changes that you must make manually](#changes-that-you-must-make-manually).

1. Verify that your code compiles and tests pass.

### Changes that the tool makes

The tool makes these changes:

- It updates literal definitions to the new syntax, such as `const one = 1n` to `const one = 1 as nat`.

- It updates the syntax for variant and option types.

- It updates parameter types, such as changing `const parameter = MyEntrypoint(value) as parameter_of MyContract;` to `const parameter = MyEntrypoint(value) as parameter_of<MyContract>;`.

- It makes punning explicit in record/object types, both in type and value definitions.

- It attempts to convert pattern matching to the new syntax but cannot do so in all cases, such as deep or complex match cases or default cases.

- It changes variables that use escaped keyword names such as `@return` to include two underscores, such as `return__`.

- It converts `do` blocks into `(() => {})` lambdas.
In this case, sometimes you must manually add variables to the scope of the lambda, as described below.

- The tool may change line breaks where they do not affect the syntax.

TODO describe changes to import syntax made.

### Converting namespaces to classes

As described in [Contract syntax](#contract-syntax), JsLIGO now allows you to define contracts in classes as well as namespaces.
If you include the `--namespace` option, the tool attempts to convert namespaces to classes.

Converting namespaces to classes is required only if the namespace implements an interface because namespaces can no longer implement interfaces in JsLIGO 2.0.

If you include the `--namespace` option, the tool makes these changes:

- Converting the namespace declaration to a class.

- Moving any type declarations in the namespace outside of the class.

- Labeling entrypoints and views as `static`.

The tool does not convert namespaces into classes in these cases:

- The namespace contains no entrypoints or views.

- The namespace has nested namespaces.
In this case the tool adds the comment `// UPGRADE: Nested namespaces need to be handled by hand.`.

Also, the tool does not comment out decorators such as `@entry` in namespaces, so you must do that manually after running the tool.

### Changes that you must make manually

After you run the upgrade tool, you must make these changes manually:

- Comment out decorators such as `@entry` in namespaces.

- Convert pattern matching to the new syntax where the tool could not do so automatically, including removing the default case if it exists.

  Also, in some cases, you may need to adjust the parameters of pattern matching cases.
  For example,

- Look for scoping errors, such as places where the tool moved a type out of a namespace where the type now conflicts with another type with the same name.

- Convert preprocessor directives manually or change your toolchain to handle them with a different preprocessor as described in [Preprocessor directives](#preprocessor-directives).

- If you aliased the module `Test.Next` to `Test` or the module `Tezos.Next` to `Tezos`, you can remove those aliases because the `*.Next` libraries have been promoted.

- Convert uses of `Test.Next.failwith` in tests to simply `failwith`.

- Correct instances where the updated syntax is not able to capture variables.
For example, assume this JsLIGO v1 code:

   ```jsligo skip
   function match_with_block() {
     let x = 1;
     return $match(["Some" as "Some", 1], {
       "None": () =>
         failwith(1),
       "Some": (org) =>
         (() =>
         { let y = x + 1; return y })(),
     });
   };
   ```

   The tool upgrades that function to the following code:

   ```jsligo skip
   function match_with_block() {
     let x = 1;
     return $match(["Some" as "Some", 1], {
       "None": () =>
         failwith(1),
       "Some": (org) =>
         (() =>
         { let y = x + 1; return y })(),
     });
   };
   ```

   The compiler throws the error `Invalid capture of mutable variable "x"` on this code because the variable `x` is not in scope of the lambda `(() => { let y = x + 1; return y })()`.
   To bring the variable into scope, add it as a parameter of the lambda, as in this example:

   ```jsligo group=lambda_scope
   function match_with_block() {
     let x = 1;
     return $match(["Some" as "Some", 1], {
       "None": () =>
         failwith(1),
       "Some": (org) =>
         ((x) =>
         { let y = x + 1; return y })(x),
     });
   };
   ```

</Syntax>