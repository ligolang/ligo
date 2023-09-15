import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

# Dynamic entrypoints

## Definition

Dynamic entrypoints are lazy entrypoints stored in the contract within a big_map. They can then be updated or removed without deploying a new contract.  
A contract with dynamic entrypoints must have at least one `@entry` declaration (as any other contract); must obey some convention on storage type definition and have at least one `@dyn_entry` declaration.  
LIGO will then include the defined dynamic entries into the contract initial storage.

### Storage

The contract storage must be a record with two fields (`storage` and `dynamic_entrypoints`).

- `storage` is your contract's storage type (as a normal contract)
- `dynamic_entrypoints` must be of type `(nat,bytes) big_map`

> **_NOTE:_** the `dynamic_entrypoints` type is defined in the standard library so you can use type puning

e.g.
<SyntaxTitle syntax="cameligo">

```
  type my_storage =
    {
     storage : int;
     dynamic_entrypoints
    }
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>




### `@dyn_entry` declaration

Dynamic entries, just like static entries must be declared in contract's top-level and have the type of an entrypoint.  

<SyntaxTitle syntax="cameligo">

```
(* define at least one entry *)
[@entry] let nop () (s: int) : operation list * int = [], s

(* define a dynamic entrypoint *)
[@dyn_entry]
let one () (_ : int) : operation list * int = [], 1
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



The dynamic entry storage type will typically coincide with the contract storage type but can be different

<SyntaxTitle syntax="cameligo">

```
[@dyn_entry]
let one_with_different_storage () (_ : nat) : operation list * nat = [], 1n
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



### Opted out dynamic entrypoints

Sometime you know an entrypoint might be defined in your contract's lifetime but you don't have any implementation yet. In this case, you can add a `@dyn_entry` declaration to your contract; give it a type and use a special expression to make ligo aware that this entry exists but should not be included in the initial storage.

<SyntaxTitle syntax="cameligo">

```
[@dyn_entry]
let opted_out : int ticket -> int -> operation list * int = [%external ("OPT_OUT_ENTRY", ())]
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



## Set and call dynamic entrypoints

Once your dynamic entrypoints defined you can now update or call them.  
One important thing is that the variables associated to dynamic entrypoints are understood by LIGO as typed keys into the `dynamic_entrypoints` big map.  
LIGO uses an abstract type `('a,'b) dynamic_entrypoint` to denote such keys.

i.e. the `one` entry defined above, would type as follow:

<SyntaxTitle syntax="cameligo">

```
let just_a_key : (unit,int) dynamic_entrypoint = one
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>




As a consequence, dynamic entrypoints are not callable:

<SyntaxTitle syntax="cameligo">

```
(* this would not type because one is not a function *)
let foo = one () 42
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



> **_NOTE:_** Of course, it is possible to make an entry callable with an intermediary function

LIGO standard library exposes three function to help you set and call your dynamic entrypoints:

<SyntaxTitle syntax="cameligo">

```
(* module Dynamic_entry *)
val set :
  ('p, 's) dynamic_entrypoint
  -> ('p, 's) entrypoint option 
  -> dynamic_entrypoints 
  -> dynamic_entrypoints

val get : 
  ('p, 's) dynamic_entrypoint
  -> dynamic_entrypoints 
  -> ('p, 's) entrypoint option

val set_bytes : 
  ('p, 's) dynamic_entrypoint
  -> bytes option
  -> dynamic_entrypoints 
  -> dynamic_entrypoints
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



### Set an entrypoint

To set an dynamic entrypoint within a static entrypoint, just use `Dynamic_entrypoints.set`:

<SyntaxTitle syntax="cameligo">

```
[@entry]
  let set_one (one_v2 : (unit, int) entrypoint) (s : storage) : operation list * storage =
    let dynamic_entrypoints =
      Dynamic_entrypoints.set one (Some one_v2) s.dynamic_entrypoints in
    [], {s with dynamic_entrypoints}
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



> **_NOTE:_** Alternatively, you can use `Dynamic_entrypoints.set_entrypoint_bytes` to set an entrypoints to its bytes encoding directly. If your encoding is wrong, any call to `Dynamic_entrypoints.get` will fail at run-time

### Get an entrypoint

To get an dynamic entrypoint within a static entrypoint and call it just use `Dynamic_entrypoints.get`:

<SyntaxTitle syntax="cameligo">

```
[@entry]
  let call_one () (s : storage) : operation list * storage =
    match Dynamic_entrypoints.get one s.dynamic_entrypoints with
      Some f ->
        let op, storage = f () s.storage in
        op, {s with storage}
    | None -> failwith (-1)
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



## Misc

### compile storage

When using `compile storage` on a contract holding dynamic entrypoints, you are expected to provide a value of your storage field type.

<SyntaxTitle syntax="cameligo">

```
> ligo compile storage dynamic_entrypoints.mligo "42"
    (Pair 42
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 1
                0x05020000002f03200931000000230765035b035b096500000008055f036d035b035b000000000200000006053d036d034200000000 })
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>



### testing

In the testing framework, you can use to 'Test.storage_with_dynamic_entrypoints' to obtain your contract initial storage. 

<SyntaxTitle syntax="cameligo">

```
let test_dyn =
  let init_storage = Test.storage_with_dynamic_entrypoints (contract_of C) 42 in
  let (addr, _, _) = Test.originate_module (contract_of  C) init_storage 0mutez in
  (* Call initial one *)
  let _ = Test.transfer_to_contract (Test.to_contract addr) (Call_one ()) 1mutez in
  let () = assert ((Test.get_storage addr).storage = 1) in
  (* Change initial one and call it *)
  let f = fun () (i : int) : operation list * int -> [], i + 1 in
  let _ = Test.transfer_to_contract (Test.to_contract addr) (Set_one f) 1mutez in
  let _ = Test.transfer_to_contract (Test.to_contract addr) (Call_one ()) 1mutez in
  let () = assert ((Test.get_storage addr).storage = 2) in
  ()
```

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
TODO
</SyntaxTitle>
