---
id: introduction
title: Introduction To LIGO
---

LIGO is a programming language for writing [Tezos](https://tezos.com/) smart contracts.

The LIGO philosophy can be described in a few bullet points:

1. Design a clean, simple language with no unnecessary parts.

2. Give that language multiple syntaxes borrowed from other languages. Ideally
LIGO requires the user to learn almost no new syntax.

3. Have that simple language encourage people to write simple code, so that it's
easy to formally verify the compiled output using a project like [Mi-Cho-Coq](https://gitlab.com/nomadic-labs/mi-cho-coq/).

4. Stop waking up in the morning to find that your smart contract lost all its money
to some [stupid exploit](https://www.wired.com/2016/06/50-million-hack-just-showed-dao-human/).

The current trend in language design is more, more, more, so this might seem
counterintuitive. However most useful smart contracts can express their core
functionality in under a thousand lines of code. The problem domain
necessitates that you get as close as possible to the minimal code size and
resource use. LIGO is a functional language designed to include the features
you need, while avoiding patterns that make formal verification hard.

For example LIGO doesn't use an object oriented paradigm, currently code is
organized using functions. While LIGO plans to have a module system in the
future, we don't want programs to have thousands of dependencies. Once a
contract is put on the blockchain, it's not possible to change it. A new
version can be uploaded, but the original contract remains available. Contracts
tend to control money. In that environment it's not responsible to encourage
users to incorporate lots of code from strangers. The immutability of contracts
make formal verification of contract logic attractive. The eventual goal is
to make it easy to compile a LIGO contract to a format that can be loaded into
the [Coq proof assistant](https://coq.inria.fr/).

## LIGO for Programming Smart Contracts on Tezos

Perhaps the most striking feature of LIGO is that it comes in
different concrete syntaxes, and even different programming
paradigms. In other words, LIGO is not defined by one syntax and one
paradigm, like imperative versus functional.

  - There is **PascaLIGO**, which is inspired by Pascal, hence is an
    imperative language with lots of keywords, where values can be
    locally mutated after they have been annotated with their types
    (declaration).

  - There is **CameLIGO**, which is inspired by the pure subset of
    [OCaml](https://ocaml.org/), hence is a functional language with
    few keywords, where values cannot be mutated, but still require
    type annotations (unlike OCaml, whose compiler performs almost
    full type inference).

  - There is **ReasonLIGO**, which is inspired by the pure subset of
    [ReasonML](https://reasonml.github.io/), which is based upon
    OCaml.

Let's define some LIGO contract in the three flavours above. Do
not worry if it is a little confusing at first; we will explain all
the syntax in the upcoming sections of the documentation.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    Increment (n) -> store + n
  | Decrement (n) -> store - n
  | Reset         -> 0
 end)
```

<!--CameLIGO-->
```cameligo group=a
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

let main (action, store : parameter * storage) : return =
  ([] : operation list),
  (match action with
     Increment n -> store + n
   | Decrement n -> store - n
   | Reset       -> 0)
```

<!--ReasonLIGO-->
```reasonligo group=a
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) : return => {
  (([] : list (operation)),
  (switch (action) {
   | Increment (n) => store + n
   | Decrement (n) => store - n
   | Reset         => 0}));
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

This LIGO contract accepts the following LIGO expressions:
`Increment(n)`, `Decrement(n)` and `Reset`. Those serve as
`entrypoint` identification.

---

## Runnable code snippets & exercises

Some of the sections in this documentation will include runnable code snippets and exercises. Sources for those are available at
the [LIGO Gitlab repository](https://gitlab.com/ligolang/ligo).

### Snippets
For example **code snippets** for the *Types* subsection of this doc, can be found here:
`gitlab-pages/docs/language-basics/src/types/**`

### Exercises
Solutions to exercises can be found e.g. here:  `gitlab-pages/docs/language-basics/exercises/types/**/solutions/**`

### Running snippets / exercise solutions
In certain cases it makes sense to be able to run/evaluate the given snippet or a solution, usually there'll be an example command which you can use, such as:

```shell
ligo evaluate-value -s pascaligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo age
# Outputs: 25
```
