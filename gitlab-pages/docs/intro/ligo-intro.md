---
id: introduction
title: Introduction To LIGO
---

LIGO is a programming language for writing [Tezos](https://tezos.com/) smart contracts.

The LIGO philosophy can be described in a few bullet points:

1. Design a clean, simple language with no extraneous parts or additions.

2. Give that language multiple syntaxes borrowed from other languages so users don't
have to clutter their brain with Yet Another Notation for the same programming
concepts we've been using since the 1980's.

3. Have that simple language encourage people to write simple code, so that it's
easy to formally verify the compiled output using a project like [Mi-Cho-Coq](https://gitlab.com/nomadic-labs/mi-cho-coq/).

4. Stop waking up in the morning to find that your smart contract lost all its money
to some [stupid exploit](https://www.wired.com/2016/06/50-million-hack-just-showed-dao-human/).

Lets expand on each:

* **Clean and simple** — Programming languages for making video games or websites
tend to prioritize accumulating a vast number of features, they're big languages for
making big projects. You use an army of mediocre programmers to write thousands
and thousands of lines of code, and then accelerate development by including dozens
or hundreds of unvetted dependencies; each of which provides an opportunity to
introduce a security exploit or insert malicious code. That's fine for a game, but
we don't think that's a very intelligent way to write a smart contract. Most useful
smart contracts can express their core functionality in under a thousand lines of
code, and the problem domain necessitates that you get as close as possible to
the minimal code size and resource use. LIGO is a functional language designed
to include the features you need, avoiding patterns that make formal verification
hard.

* **Multiple Syntaxes** — LIGO provides three syntaxes for users which express the
same underlying language semantics. PascaLIGO is an imperative syntax based on
[Pascal](https://en.wikipedia.org/wiki/Pascal_%28programming_language%29), CameLIGO
is a syntax which closely mimics the look and feel of [OCaml](https://en.wikipedia.org/wiki/OCaml),
and ReasonLIGO is based on Facebook's JavaScript-flavored [ReasonML](https://reasonml.github.io/) syntax.

* **Simple Code & Formal Verification** — LIGO doesn't use an object oriented paradigm,
currently code is organized using functions. While LIGO plans to have a module system
in the future, it's not the intent that this be used to create npm style cathedrals
of logical mystery meat. Once a contract is put on the blockchain, it's not possible
to change it. A new version can be uploaded, but the original contract remains
available. This makes formal verification of contract logic attractive. When the
cost of bugs is extreme and patches aren't possible it pays to get things right
the first time.

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
