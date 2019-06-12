---
id: cheat-sheet
title: Cheat Sheet
---

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

|Primitive   	|Example|
|---	|---|
|Strings | `"Tezos"`|
|Characters | `"t"`|
|Integers | `42`, `7`|
|Natural numbers | `42n`, `7n`|
|Unit| `unit`|
|Boolean|<pre><code>const hasDriversLicense: bool = False;<br/>const adult: bool = True;</code></pre> |
|Mutez (micro tez)| `42mtz`, `7mtz` |
|Address | `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`, `"KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD"`|
|Addition |`3 + 4`, `3n + 4n`|
|Multiplication & Division| `3 * 4`, `3n * 4n`, `10 / 5`, `10n / 5n`|
|Modulo| `10 mod 3`|
|Tuples| <pre><code>type name is (string * string);<br/>const winner: name = ("John", "Doe");<br/>const firstName: string = winner.0;<br/>const lastName: string = winner.1;</code></pre>|
|Types|`type age is int`, `type name is string`  |
|Includes|```#include "library.ligo"```|
|Functions (short form)|<pre><code>function add (const a : int ; const b : int) : int is<br/>&nbsp;&nbsp;block { skip } with a + b</code></pre>|
|Functions (long form)|<pre><code>function add (const a : int ; const b : int) : int is<br/>&nbsp;&nbsp;block { <br/>&nbsp;&nbsp;&nbsp;&nbsp;const result: int = a + b;<br/>&nbsp;&nbsp;} with result</code></pre>|
|Options|<pre><code>type middleName is option(string);<br/>const middleName : middleName = Some("Foo");<br/>const middleName : middleName = None;</code></pre>|
|Assignment| ```const age: int = 5;```|
|Assignment on an existing variable <br/></br>*⚠️ This feature is not supported at the top-level scope, you can use it e.g. within functions. Works for Records and Maps as well.*| ```age := 18;```, ```p.age := 21``` |
|Annotations| ```("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)```|
|Variants|<pre><code>type action is<br/>&#124; Increment of int<br/>&#124; Decrement of int</code></pre>|
|Variant *(pattern)* matching|<pre><code>const a: action = Increment(5);<br/>case a of<br/>&#124; Increment n -> n + 1<br/>&#124; Decrement n -> n - 1<br/>end</code></pre>|
|Records|<pre><code>type person is record<br/>&nbsp;&nbsp;age: int ;<br/>&nbsp;&nbsp;name: string ;<br/>end<br/><br/>const john : person = record<br/>&nbsp;&nbsp;age = 18;<br/>&nbsp;&nbsp;name = "John Doe";<br/>end<br/><br/>const name: string = john.name;</code></pre>|
|Maps|<pre><code>type prices is map(nat, tez);<br/><br/>const prices : prices = map<br/>&nbsp;&nbsp;10n -> 60mtz;<br/>&nbsp;&nbsp;50n -> 30mtz;<br/>&nbsp;&nbsp;100n -> 10mtz;<br/>end<br/><br/>const price: option(tez) = prices[50n];</code></pre>|


<!--END_DOCUSAURUS_CODE_TABS-->