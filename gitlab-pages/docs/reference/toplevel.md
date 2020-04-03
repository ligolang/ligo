---
id: toplevel
title: Toplevel
description: Available functions at the top level
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

These functions are available without any needed prefix.

<SyntaxTitle syntax="pascaligo">
function is_nat: int -> option(nat)
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
val is_nat: int -> nat option
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
let is_nat: int => option(nat)
</SyntaxTitle>

Convert an `int` to a `nat` if possible.

Note that `Michelson.is_nat` is deprecated. Please use `is_nat` instead.

<SyntaxTitle syntax="pascaligo">
function abs: int -> nat
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
val abs: int -> nat
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
let abs: int => nat
</SyntaxTitle>

Cast an `int` to `nat`.

<SyntaxTitle syntax="pascaligo">
function int: nat -> int
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
val int: nat -> int
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
let int: nat => int
</SyntaxTitle>

Cast an `nat` to `int`.

<SyntaxTitle syntax="pascaligo">
const unit: unit
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
val unit: unit
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
let (): unit
</SyntaxTitle>

A helper to create a unit.

<SyntaxTitle syntax="pascaligo">
function failwith : string -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
function failwith : string -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
function failwith : string -> unit
</SyntaxTitle>

Cause the contract to fail with an error message.

> ⚠ Using this currently requires in general a type annotation on the
> `failwith` call.

<SyntaxTitle syntax="pascaligo">
function assert : bool -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
function assert : bool -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
function assert : bool -> unit
</SyntaxTitle>

Check if a certain condition has been met. If not the contract will fail.
