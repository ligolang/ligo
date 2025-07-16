---
id: conditionals
title: Conditionals
---

LIGO includes logical `if` and `else` statements like many other languages.

<Syntax syntax="cameligo">

```cameligo group=if
let greater_than_5 (input : int) : bool =
  if input > 5 then true else false
```

:::note

As in OCaml, in CameLIGO, if a conditional has a branch `else ()`, that branch can be omitted.
The resulting so-called *dangling else* problem is parsed by associating any `else` to the closest previous `then`.

:::

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=if
function greater_than_5(input: int) {
  if (input > 5) {
    return true;
  } else {
    return false;
  }
}
```

</Syntax>

<Syntax syntax="jsligo">

## Switch statement

JsLIGO also supports branching of control flow via the `switch` statement.

```jsligo group=switch
let quarter = n => {
  let output = "";
  switch (n) {
    case 1:
    case 2:
    case 3:
      output = "Q1";
      break;
    case 4:
    case 5:
    case 6:
      output = "Q2";
      break;
    case 7:
    case 8:
    case 9:
      output = "Q3";
      break;
    case 10:
    case 11:
    case 12:
      output = "Q4";
      break;
    default:
      output = "Invalid month."
  };
  return output;
}
```

The `switch` statement takes an expression and tries to find a case that matches the expression.
If a matching case is found, the statements of the matching case are executed until a `break;` statement.
If no `break` statement is found, LIGO continues to the next case or `default` case.
If no matching case is found, LIGO runs the statements in the `default` case.

In LIGO, the `switch` statement has these limitations:

- The `switch` statement must have at least one case or `default` case.
- If a `default` case is provided, it should be the last case.
- Conditional `break` statements are not supported; for example, you can't include a `break` statement inside a `if-then-else` block.
- In the case of nested `switch` statements, the inner `switch` statement should not contain a `return` statement.

You can run the `quarter` function defined above using the LIGO compiler like this:

```shell
ligo run evaluate-call  gitlab-pages/docs/imperative/src/conditionals/switch.jsligo quarter '5'
# Outputs: "Q2"
```

## Ternary conditional expression

JsLIGO also supports JavaScript's ternary expression:

```jsligo group=ternary
const ternary = a => (a == 1) ? true : false;
```

Ternary expressions can be nested, as in this example:

```jsligo group=ternary
const ternary_nested = a =>
  a == 1 ? "one"   :
  a == 2 ? "two"   :
  a == 3 ? "three" :
           "other"
```

</Syntax>

<!-- updated use of entry -->