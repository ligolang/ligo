---
id: michelson-injection
title: Michelson injection
---

import Syntax from '@theme/Syntax';

If you have an existing piece of Michelson code that you want to use
as-is, LIGO provides the ability to embed (inject) Michelson
code. This feature can be useful when you need to have a deep level of
control over the generated code, for example for optimisation, or if
you need to use a feature from Michelson that is not yet supported by
high-level constructions in LIGO.

### Internal injection

<Syntax syntax="cameligo">

The syntax for embedding Michelson is by means of the
`[%Michelson ...]` special hook. The ellipsis is meant to denote a
verbatim string annotated with a type, which contains the Michelson
code to be injected in the generated Michelson and the type (that of a
function) of the Michelson code.

```cameligo group=michelson_inj
let michelson_add n =
  [%Michelson ({| { UNPAIR ; ADD } |} : nat * nat -> nat)] n
```

</Syntax>

<Syntax syntax="jsligo">

The syntax for embedding Michelson is by means of the `(Michelson
...)` hook. The ellipsis is meant to denote a verbatim string
annotated with a type, which contains the Michelson code to be
injected in the generated Michelson and the type (that of a function)
of the Michelson code.


```jsligo group=michelson_inj
const michelson_add = n =>
  (Michelson `{ UNPAIR ; ADD }` as ((n: [nat, nat]) => nat))(n);
```

</Syntax>

Note that the type annotation is required, because the embedded
Michelson code is not type-checked by the LIGO compiler, which
therefore assumes that the given type is correct.

<Syntax syntax="cameligo">

In the example above, the notation ```{| ... |}``` is used to
represent a verbatim string literal, that is, an uninterpreted string,
which here contains a piece of Michelson code. The type annotation
describes the behaviour of the Michelson code:

</Syntax>

<Syntax syntax="jsligo">

In the example above, the notation `` ` ... ` `` is used to represent a
verbatim string literal, that is, an uninterpreted string, which here
contains a piece of Michelson code. The type annotation describes the
behaviour of the Michelson code:

</Syntax>

- It starts working on a stack consisting of a tuple of `nat`s: `[ nat * nat ]`.

- The tuple is destructured using `UNPAIR`: `[ nat ] [ nat ]`.

- The two top values of the stack are added using `ADD`,
  and stops working on a stack consisting of a single `nat`: `[ nat ]`.

The compiler will prevent changes to the embedded Michelson code if
the function resulting from the embedded code is not applied. For
example, let us see what happens when we compile an embedded Michelson
expression that pushes some value on the stack, then drops it
immediately, and then continues as a regular increment function.

<Syntax syntax="cameligo">

The following command-line:

```shell
ligo compile expression cameligo "[%Michelson ({| { PUSH nat 42; DROP; PUSH nat 1; ADD } |} : nat -> nat)]"
```

outputs:
> { PUSH nat 42 ; DROP ; PUSH nat 1 ; ADD }


</Syntax>

<Syntax syntax="jsligo">

The following command-line:

```shell
ligo compile expression jsligo "(Michelson `{ PUSH nat 42; DROP; PUSH nat 1; ADD }` : nat -> nat)"
```

outputs:
> { PUSH nat 42 ; DROP ; PUSH nat 1 ; ADD }


</Syntax>

As we can see, the embedded Michelson code was not modified. However,
if the resulting function is applied, then the embedded Michelson code
could be modified/optimised by the compiler. To demonstrate this
behaviour, a function call can be introduced in the example above by
creating a lambda around the Michelson code. In this case, the first
two instructions will be removed by the LIGO compiler because they
have no effect on the final result.

<Syntax syntax="cameligo">

The following command-line:

```shell
ligo compile expression cameligo "fun n -> [%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : nat -> nat)] n"
```

outputs:
> { PUSH nat 1 ; ADD }

</Syntax>

<Syntax syntax="jsligo">

The following command-line:

```shell
ligo compile expression jsligo "fun n -> (Michelson `{ PUSH nat 42; DROP ; PUSH nat 1; ADD }` : nat -> nat) n"
```

outputs:
> { PUSH nat 1 ; ADD }

</Syntax>

### External injection

Sometimes the Michelson code we wish to inject is better maintained
externally, perhaps by a third-party, in which case we need to load
the Michelson code in order to inject it.

<Syntax syntax="cameligo">

This is achieved by the special hook `[%of_file ...]`, where the
ellipsis is a string containing a file path to a Michelson file with
extension `.tz`.

</Syntax>

<Syntax syntax="jsligo">

This is achieved by the special hook `(of_file ...)`, where the
ellipsis is a *verbatim* string containing a file path to a Michelson
file with extension `.tz`.

</Syntax>

### Injection of Michelson contracts

<Syntax syntax="cameligo">

This is achieved by the special hook `[%create_contract_of_file ...]`,
where the ellipsis is a string containg the file path to a Michelson
file with extension `.tz`.

```cameligo group=michelson_inj
[@entry]
let main (param : unit) () : operation list * unit =
  let op, _addr =
    [%create_contract_of_file "gitlab-pages/docs/tezos/contracts/src/compiled.tz"]
    None 1tez param
  in [op], ()
```

</Syntax>

<Syntax syntax="jsligo">

This is achieved by the special hook `(create_contract_of_file ...)`,
where the ellipsis is a *verbatim* string containg the file path to a
Michelson file with extension `.tz`.

```jsligo group=michelson_inj
@entry
const main = (param: unit, _storage: unit) : [list<operation>, unit] => {
  const [op, _addr] =
    (create_contract_of_file `gitlab-pages/docs/tezos/contracts/src/compiled.tz`)
    (None(), 1tez, param)
  return [[op], []];
}
```

</Syntax>

where `compiled.tz` contains

```michelson
{ parameter unit ;
  storage unit ;
  code { DROP ; UNIT ; NIL operation ; PAIR } }
```
