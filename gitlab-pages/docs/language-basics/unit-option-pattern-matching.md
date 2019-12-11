---
id: unit-option-pattern-matching
title: Unit, Option, Pattern matching
---

Optionals are a programing pattern seen in OCaml. Since Michelson and LIGO are both inspired by OCaml, you'll have the *option* to use them in LIGO as well.

## Type unit

Units in Michelson or LIGO represent *for the lack of better words* - an empty/useless/not needed value.

Here's how they're defined:

> 💡 Units come in handy when we try pattern matching on custom variants below.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const n: unit = Unit;
```

<!--Cameligo-->
```cameligo
let n: unit = ()
```

<!--ReasonLIGO-->
```reasonligo
let n: unit = ();
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Variants

Variant is a user-defined or a built-in type (in case of optionals) that can be compared to Enum (from javascript).

Here's how to define a new variant type:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type id is nat
type user is
| Admin of id
| Manager of id
| Guest;

const u: user = Admin(1000n);
const g: user = Guest(Unit);
```

<!--Cameligo-->
```cameligo
type id = nat
type user =
| Admin of id
| Manager of id
| Guest of unit

let u: user = Admin 1000n
let g: user = Guest ()
```

<!--ReasonLIGO-->
```reasonligo
type id = nat;
type user =
  | Admin(id)
  | Manager(id)
  | Guest(unit);

let u: user = Admin(1000n);
let g: user = Guest();
```

<!--END_DOCUSAURUS_CODE_TABS-->

Defining a varient can be extremely useful for building semantically appealing contracts. We'll learn how to use variants for 'logic purposes' shortly.

## Optional values

Optionals are a type of built-in variant that can be used to determine if a variable holds a certain value or not. This is especially useful when (for example) your program's state allows for a certain variable value to be empty, like this:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type dinner is option(string);

// stay hungry
const p1: dinner = None;
// have some hamburgers
const p2: dinner = Some("Hamburgers")
```

<!--Cameligo-->
```cameligo
type dinner = string option

let p1: dinner = None
let p2: dinner = Some "Hamburgers"
```

<!--ReasonLIGO-->
```reasonligo
type dinner = option(string);

let p1: dinner = None;
let p2: dinner = Some("Hamburgers");
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Pattern matching

Pattern matching is very similiar to e.g. `switch` in Javascript, and can be used to re-route the program's flow based on a value of a variant. 

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type dinner is option(string);
function is_hungry(const dinner: dinner): bool is block { skip } 
    with (
        case dinner of 
            | None -> True
            | Some(d) -> False
        end
    )
```

<!--Cameligo-->
```cameligo
type dinner = string option
let is_hungry (d: dinner) : bool =
  match d with
  | None -> true
  | Some s -> false
```

<!--ReasonLIGO-->
```reasonligo
type dinner = option(string);
let is_hungry = (d: dinner): bool =>
  switch (d) {
  | None => true
  | Some(s) => false
  };
```

<!--END_DOCUSAURUS_CODE_TABS-->
