---
id: loops
title: Loops
---



## While Loop

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

The PascaLIGO while loop should look familiar to users of imperative languages. 
While loops are of the form `while <condition clause> <block>`, and evaluate
their associated block until the condition evaluates to false.

> ⚠️ The current PascaLIGO while loop has semantics that have diverged from other LIGO syntaxes. The goal of LIGO is that the various syntaxes express the same semantics, so this will be corrected in future versions. For details on how loops will likely work after refactoring, see the CameLIGO tab of this example.

```pascaligo
function while_sum (var n : nat) : nat is block {
  var i : nat := 0n ;
  var r : nat := 0n ;
  while i < n block {
    i := i + 1n;
    r := r + i;
  }
} with r
```

<!--CameLIGO-->

`Loop.fold_while` is a fold operation that takes an initial value of a certain type
and then iterates on it until a condition is reached. The auxillary function
that does the fold returns either boolean true or boolean false to indicate
whether the fold should continue or not. The initial value must match the input
parameter of the auxillary function, and the auxillary should return type `(bool * input)`.

```cameligo
let aux (i: int) : bool * int =
  if i < 100 then continue (i + 1) else stop i

let counter_simple (n: int) : int =
  Loop.fold_while aux_simple n
```

<!--END_DOCUSAURUS_CODE_TABS-->

## For Loop

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

To iterate over a range of integers you use a loop of the form `for <variable assignment> to <integer> <block>`.

```pascaligo
function for_sum (var n : nat) : int is block {
  var acc : int := 0 ;
  for i := 1 to int(n)
    begin
      acc := acc + i;
    end
} with acc
```

<!--END_DOCUSAURUS_CODE_TABS-->

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

PascaLIGO for loops can also iterate through the contents of a collection. This is
done with a loop of the form `for <element var> in <collection type> <collection var> <block>`.

```pascaligo
function for_collection_list (var nee : unit) : (int * string) is block {
  var acc : int := 0;
  var st : string := "to";
  var mylist : list(int) := list 1; 1; 1 end;
  for x in list mylist
    begin
      acc := acc + x;
      st := st ^ "to";
    end
} with (acc, st)
```

<!--END_DOCUSAURUS_CODE_TABS-->
