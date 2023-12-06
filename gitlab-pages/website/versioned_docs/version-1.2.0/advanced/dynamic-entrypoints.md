import Syntax from '@theme/Syntax';

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
<Syntax syntax="cameligo">

```cameligo skip
  type my_storage =
    {
     storage : int;
     dynamic_entrypoints
    }
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
  type my_storage =
    {
     storage : int;
     dynamic_entrypoints
    }
```

</Syntax>




### `@dyn_entry` declaration

Dynamic entries, just like static entries must be declared in contract's top-level and have the type of an entrypoint.  

<Syntax syntax="cameligo">

```cameligo skip
(* define at least one entry *)
[@entry]
let nop () (s: int) : operation list * int = [], s

(* define a dynamic entrypoint *)
[@dyn_entry]
let one () (_ : int) : operation list * int = [], 1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
// define at least one entry
@entry
const nop = ([], s: int) : [list<operation>, int] => [list([]), s]

// define a dynamic entrypoint
@dyn_entry
const one = ([], _i : int) : [list<operation>, int] => [list([]), 1]
```

</Syntax>



The dynamic entry storage type will typically coincide with the contract storage type but can be different

<Syntax syntax="cameligo">

```cameligo skip
[@dyn_entry]
let one_with_different_storage () (_: nat) : operation list * nat = [], 1n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@dyn_entry
const one_with_different_storage = ([], _n : nat) : [list<operation>, nat] => [list([]), 1n]
```

</Syntax>



### Opted out dynamic entrypoints

Sometime you know an entrypoint might be defined in your contract's lifetime but you don't have any implementation yet. In this case, you can add a `@dyn_entry` declaration to your contract; give it a type and use a special expression to make ligo aware that this entry exists but should not be included in the initial storage.

<Syntax syntax="cameligo">

```cameligo skip
[@dyn_entry]
let opted_out : int ticket -> int -> operation list * int = [%external ("OPT_OUT_ENTRY", ())]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@dyn_entry
const opted_out = (_t: ticket<int>, _i: int) : [list<operation>, int] => (External `OPT_OUT_ENTRY`)
```

</Syntax>



## Set and call dynamic entrypoints

Once your dynamic entrypoints defined you can now update or call them.  
One important thing is that the variables associated to dynamic entrypoints are understood by LIGO as typed keys into the `dynamic_entrypoints` big map.
<Syntax syntax="cameligo">

LIGO uses an abstract type `('a,'b) dynamic_entrypoint` to denote such keys.

</Syntax>
<Syntax syntax="jsligo">

LIGO uses an abstract type `dynamic_entrypoint<a, b>` to denote such keys.

</Syntax>

i.e. the `one` entry defined above, would type as follow:

<Syntax syntax="cameligo">

```cameligo skip
let just_a_key : (unit,int) dynamic_entrypoint = one
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
let just_a_key : dynamic_entrypoint<unit, int> = one
```

</Syntax>




As a consequence, dynamic entrypoints are not callable:

<Syntax syntax="cameligo">

```cameligo skip
(* this would not type because one is not a function *)
let foo = one () 42
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
(* this would not type because one is not a function *)
let foo = one([], 42)
```

</Syntax>



> **_NOTE:_** Of course, it is possible to make an entry callable with an intermediary function

LIGO standard library exposes three function to help you set and call your dynamic entrypoints:

<Syntax syntax="cameligo">

```cameligo skip
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

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
(* module Dynamic_entry *)
const set : <P, S>(x1: dynamic_entrypoint<P, S>, x2: option<entrypoint>, x3: dynamic_entrypoints) => dynamic_entrypoints

const get : <P, S>(x1: dynamic_entrypoint<P, S>, x2: dynamic_entrypoints) => option<dynamic_entrypoint<P, S>>

const set_bytes : <P, S>(x1: dynamic_entrypoint<P, S>, x2: option<bytes>, x3: dynamic_entrypoints) => dynamic_entrypoints
```

</Syntax>



### Set an entrypoint

To set an dynamic entrypoint within a static entrypoint, just use `Dynamic_entrypoints.set`:

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let set_one (one_v2 : (unit, int) entrypoint) (s : storage) : operation list * storage =
  let dynamic_entrypoints =
    Dynamic_entrypoints.set one (Some one_v2) s.dynamic_entrypoints in
  [], {s with dynamic_entrypoints}
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@entry
const set_one = (one_v2 : entrypoint<unit, int>, s : storage) : [list<operation>, storage] => {
  let dynamic_entrypoints =
    Dynamic_entrypoints.set(one, Some(one_v2), s.dynamic_entrypoints);
  return [list([]), {...s, dynamic_entrypoints}]
}
```

</Syntax>



> **_NOTE:_** Alternatively, you can use `Dynamic_entrypoints.set_entrypoint_bytes` to set an entrypoints to its bytes encoding directly. If your encoding is wrong, any call to `Dynamic_entrypoints.get` will fail at run-time

### Get an entrypoint

To get an dynamic entrypoint within a static entrypoint and call it just use `Dynamic_entrypoints.get`:

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let call_one () (s : storage) : operation list * storage =
  match Dynamic_entrypoints.get one s.dynamic_entrypoints with
    Some f ->
      let op, storage = f () s.storage in
      op, {s with storage}
  | None -> failwith (-1)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@entry
const call_one = ([], s : storage) : [list<operation>, storage] =>
  match (Dynamic_entrypoints.get(one, s.dynamic_entrypoints)) {
    when (Some(f)): do {
      const [op, storage] = f([], s.storage);
      return [op, ({...s, storage})]
    };
    when (None): failwith(-1);
  }
```

</Syntax>



## Misc

### compile storage

When using `compile storage` on a contract holding dynamic entrypoints, you are expected to provide a value of your storage field type.

<Syntax syntax="cameligo">

```zsh
> ligo compile storage dynamic_entrypoints.mligo "42"
    (Pair 42
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 1
                0x05020000002f03200931000000230765035b035b096500000008055f036d035b035b000000000200000006053d036d034200000000 })
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
> ligo compile storage dynamic_entrypoints.jsligo "42"
    (Pair 42
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 1
                0x05020000002f03200931000000230765035b035b096500000008055f036d035b035b000000000200000006053d036d034200000000 })
```

</Syntax>



### testing

In the testing framework, you can use to 'Test.storage_with_dynamic_entrypoints' to obtain your contract initial storage. 

<Syntax syntax="cameligo">

```cameligo skip
let test_dyn =
  let init_storage = Test.storage_with_dynamic_entrypoints (contract_of C) 42 in
  let (addr, _, _) = Test.originate (contract_of  C) init_storage 0mutez in
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

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const test_dyn = do {
  const init_storage = Test.storage_with_dynamic_entrypoints(contract_of(C), 42);
  const [addr, _init, _balance] = Test.originate (contract_of(C), init_storage, 0mutez);
  /* Call initial one */
  Test.transfer_to_contract (Test.to_contract(addr), Call_one(), 1mutez);
  assert ((Test.get_storage(addr)).storage == 1);
  /* Change initial one and call it */
  const f = (_unit : unit, i : int) : [list<operation>, int] => [list([]), i + 1];
  Test.transfer_to_contract (Test.to_contract(addr), (Set_one(f)), 1mutez);
  Test.transfer_to_contract (Test.to_contract(addr), (Call_one()), 1mutez);
  assert ((Test.get_storage(addr)).storage == 2);
  return []
}
```

</Syntax>
