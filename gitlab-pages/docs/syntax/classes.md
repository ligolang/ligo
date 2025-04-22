---
title: Classes
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Classes are not supported in CameLIGO.

</Syntax>

<Syntax syntax="jsligo">

Classes (introduced in JsLIGO 2.0) are similar to classes in JavaScript/Typescript.
You can use them to define a smart contract or group related functions.

JsLIGO classes do not have all of the same features as JavaScript/TypeScript classes.
For example, JsLIGO classes cannot inherit from other classes as they can in JavaScript/TypeScript.
However, classes can implement [interfaces](./signatures).

## Creating classes

JsLIGO classes can contain only function definitions, which are referred to as _properties_ when they are in a class.
Because each property is assumed to be a function, you do not prefix definitions with the `function`, `const`, or `let` keywords.
Within a class, you can also apply decorators such as `@entry` to functions without putting them in comments.

Classes are one way to define contracts, as in this example:

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

:::tip

Classes cannot contain type definitions, only functions.
To encapsulate classes with types, such as if you want to define a type that represents the contract storage or types that represent complex parameter types, you can group the contract class and the related types in a namespace.

:::

## Importing and using classes

You can import and use definitions from classes in the same file or in different files.
For example, this version of the previous example uses functions that are in another class and types that are in another namespace:

```jsligo group=import_classes_local
namespace MyTypes {
  export type storage_type = int;
  export type return_type = [list<operation>, storage_type];
}

class MyFunctions {
  static add = (a: int, b: int): int => a + b;
  static sub = (a: int, b: int): int => a - b;
}

class Counter {
  @entry
  add = (value: int, storage: MyTypes.storage_type): MyTypes.return_type =>
    [[], MyFunctions.add(storage, value)];

  @entry
  sub = (value: int, storage: MyTypes.storage_type): MyTypes.return_type =>
    [[], MyFunctions.add(storage, value)];
}
```

To make properties accessible outside of a class, you must mark them with the `static` keyword or the `@public` decorator.
The `static` keyword is more appropriate because external code accesses the definitions of the class in a static manner, without creating an instance of the class.

For example, this file exports a class with static properties:

```jsligo group=exported_class
export class MyFunctions {
  static add = (a: int, b: int): int => a + b;
  static sub = (a: int, b: int): int => a - b;
}
```

This file imports and uses the class:

```jsligo group=imports_classes
type storage_type = int;
type return_type = [list<operation>, storage_type];

import * as MyImportedFunctions from "./exported_class.jsligo";

class Calculator {

  @entry
  add = (value: int, storage: storage_type): return_type =>
    [[], MyImportedFunctions.MyFunctions.add(storage, value)];

  @entry
  sub = (value: int, storage: storage_type): return_type =>
    [[], MyImportedFunctions.MyFunctions.sub(storage, value)];

}
```

</Syntax>
