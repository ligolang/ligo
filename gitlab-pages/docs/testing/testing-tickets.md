---
id: testing-tickets
title: Testing tickets
---

Testing tickets requires some extra steps.

## The problem with testing tickets

The Tezos protocol has two types of operations:

- Internal operations are created from inside the chain, such as by smart contracts.
- External operations are created from outside the chain, such as by the Octez client, other Tezos clients and SDKs, and in the context of LIGO testing, by functions such as `Test.Next.Originate.contract`.
For more information about the technical implementation, see [Semantics of smart contracts and transactions](https://tezos.gitlab.io/active/michelson.html#semantics-of-smart-contracts-and-transactions) in the Octez documentation.

In the protocol, both external and internal origination and transfer operations contain a piece of michelson code representing the initial storage for the origination or the parameter for the transfer.

Now imagine you have a value of type `parameter_ty`/`storage_ty` containing a ticket, that you want to transfer or originate,
in the operation data, tickets will be represented in Michelson as pairs:

```bash
> ligo compile expression cameligo 'Tezos.create_ticket 0x0202 10n'
(Pair "KT1DUMMYDUMMYDUMMYDUMMYDUMMYDUMu2oHG" 0x0202 10)
```

> ticketer address , ticket value , ticket amount

If we try to apply such an operation, the type wouldn't match: ticket of bytes VS some pair.
The protocol would not let you do that since you could be creating a ticket out of nowhere unless the operation happens to be forged from within a contract (i.e. "internally")!

In the testing framework - for now - it means using "proxy-contracts" forging the operations using provided a ticket value and a ticket amount.

## Proxy ticket contracts

The LIGO standard library provides a `Proxy_ticket` module which helps in working with tickets in the Testing framework. Here is the interface of `Proxy_ticket`:

---
`init_transfer` accepts:

* a function `mk_param` which given a ticket must return a value of your parameter type

and returns the typed address of a "transfer proxy-contract" which can then be used to do multiple transfers of tickets with the same ticketer address

---

`transfer` accepts :

* the typed address of a "transfer proxy-contract"
* the ticket information (value and amount) together with the destination address

and returns a value of type `test_exec_result`

---

`originate` accepts:

* the ticket information (value and amount)
* a function `mk_storage` which given a ticket must return a value of your storage type
* your contract (having a ticket in its storage type)

---

> Note: Functions `mk_param` and `mk_storage` will be executed in the proxy contract itself

Find more detailed information on the API of [`Proxy_ticket` here](../reference/test.proxy_ticket.md)

## Usages

### Transfer

Here is an example using `Proxy_ticket.init_transfer` and `Proxy_ticket.transfer`:

1. import the module above as `Proxy_ticket`
2. define a contract `C` holding a ticket of string in its parameter type. The contract will just store the value of
  the received ticket and the address of the sender
3. originate contract `C`
4. initialize a "transfer proxy-contract" providing a function to build a parameter out of a ticket
5. transfer a ticket with a value `"hello"` and an amount of `10` to contract `C`
6. print the storage of contract `C`
7. transfer a ticket with a value `"world"` and an amount of `5` to contract `C`
8. print the storage of contract `C`

<Syntax syntax="cameligo">

```cameligo test-ligo group=usage_transfer
module C = struct
  type param = int * string ticket
  type storage = string * address

  [@entry]
  let main (p : param) (_ : storage) : operation list * storage =
    let (_,ticket) = p in
    let (_,(v,_)) , _ = Tezos.read_ticket ticket in
    [] , (v, Tezos.get_sender ())
end
let test_transfer_to_contract =
  let {addr = main_taddr; code = _ ; size = _} = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
  let main_addr = Test.to_address main_taddr in

  (* Use this address everytime you want to send tickets from the same proxy-contract *)
  let proxy_taddr =
    (* mk_param is executed __by the proxy contract__ *)
    let mk_param : string ticket -> C.param = fun (t : string ticket) -> 42,t in
    (* initialize a proxy contract in charge of creating and sending your tickets *)
    Test.Proxy_ticket.init_transfer mk_param
  in
  let () = Test.log ("poxy addr:", proxy_taddr) in

  let _ =
    (* ticket_info lets you control the amount and the value of the tickets you send *)
    let ticket_info = ("hello", 10n) in
    (* we send ticket to C through the proxy-contract *)
    Test.Proxy_ticket.transfer proxy_taddr (ticket_info,main_addr)
  in
  let () = Test.log (Test.get_storage main_taddr) in
  let _ =
    let ticket_info = ("world",5n) in
    Test.Proxy_ticket.transfer proxy_taddr (ticket_info,main_addr)
  in
  let () = Test.log (Test.get_storage main_taddr) in
  ()
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo test-ligo group=usage_transfer
namespace C {
  export type param = [ int , ticket<string>]

  @entry
  function main (p: param, _s: [string , address]) : [list<operation> , [string , address]] {
    let [_v,ticket] = p ;
    let [[_addr,[v,_t]] , _ticket] = Tezos.read_ticket (ticket) ;
    return ([[] , [v, Tezos.get_sender ()]])
  };
}

const test_transfer_to_contract = do {
  let {addr : main_taddr, code , size } = Test.originate (contract_of(C), ["bye",Test.nth_bootstrap_account (1)], 1mutez) ;
  let main_addr = Test.to_address (main_taddr) ;

  /* mk_param is executed __by the proxy contract__ */
  const mk_param = (t:ticket<string>) : C.param => { return [42,t] } ;
  /* Use this address everytime you want to send tickets from the same proxy-contract */
  /* initialize a proxy contract in charge of creating and sending your tickets */
  let proxy_taddr = Test.Proxy_ticket.init_transfer (mk_param) ;
  Test.log (["poxy addr:", proxy_taddr]) ;

  /* ticket_info lets you control the amount and the value of the tickets you send */
  let ticket_info1 = ["hello", 10n];
  /* we send ticket to main through the proxy-contract */
  Test.Proxy_ticket.transfer (proxy_taddr, [ticket_info1,main_addr]) ;
  Test.log (Test.get_storage (main_taddr)) ;

  let ticket_info2 = ["world",5n] ;
  Test.Proxy_ticket.transfer (proxy_taddr, [ticket_info2,main_addr]) ;
  Test.log (Test.get_storage (main_taddr));
};
```

</Syntax>

result:

```bash
> ligo run test transfer_ticket.mligo
("poxy addr:" , KT1QGANLjYsyJmw1QNww9Jkgb4ccQr6W2gsC)
("hello" , KT1QGANLjYsyJmw1QNww9Jkgb4ccQr6W2gsC)
("world" , KT1QGANLjYsyJmw1QNww9Jkgb4ccQr6W2gsC)
Everything at the top-level was executed.
- test_transfer_to_contract exited with value ().
```

> Note: note that the sender (stored in the contract) matches the address of the proxy contract

### Origination

Here is an example using `Proxy_ticket.originate` and the type `unforged_ticket` :

1. import the module above as `Proxy_ticket`
2. define a contract `main` potentially holding a ticket of bytes in its storage. The contract will just reads the ticket
   in its storage if present. Note that we define two version of the contract storage type: one for the contract
   and one for the storage type that we would like to manipulate in our testing logic
3. we define the `mk_storage` function which simply wraps a ticket into an option type
4. we define the ticket information for a ticket of value `0x0202` and an amount of `15`
5. we call `originate` and retrieve the address of the newly originated contract
6. we use the address to fetch the current contract storage using `Test.get_storage_of_address` and decompile it
   as a `human_storage`
7. we read the content of the ticket and perform a series of assertions

<Syntax syntax="cameligo">

```cameligo test-ligo group=usage_orig
(* originate.mligo *)

type storage = (bytes ticket) option
type unforged_storage = (bytes unforged_ticket) option

let main (() : unit) (s : storage) : operation list * storage =
  [] , (
    match s with
    | Some ticket ->
      let (_ , t) = Tezos.read_ticket ticket in
      Some t
    | None -> None
  )

let test_originate_contract =
  let mk_storage = fun (t:bytes ticket) -> Some t in
  let ticket_info = (0x0202, 15n) in
  let addr = Test.Proxy_ticket.originate ticket_info mk_storage main in
  let unforged_storage : unforged_storage = Test.Proxy_ticket.get_storage addr in

  (* the ticket 'unforged_storage' can be manipulated freely without caring about ticket linearity *)

  match unforged_storage with
  | Some { ticketer ; value ; amount } ->
    let () = Test.log ("unforged_ticket", unforged_storage) in
    let () = assert (value = ticket_info.0) in
    let () = assert (amount = ticket_info.1) in
    ()
  | None -> failwith "impossible"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=usage_orig
type storage = option< ticket<bytes> >
type unforged_storage = option< unforged_ticket<bytes> >

const main = (_p: unit, s: storage) : [ list<operation> , storage] => {
  let x =
    match (s) {
      when(Some(ticket)): ((ticket: ticket<bytes>) => {
        let [_v , t] = Tezos.read_ticket (ticket) ;
        return Some (t)
      })(ticket);
      when(None()): None()
    };
  return [list ([]), x]
};

const test_originate_contract = do {
  const mk_storage = (t:ticket<bytes>) : storage => { return (Some (t)) } ;
  let ticket_info = [0x0202, 15n];
  let addr = Test.Proxy_ticket.originate (ticket_info, mk_storage, main) ;
  let unforged_storage = (Test.Proxy_ticket.get_storage (addr) as unforged_storage) ;

  /* the ticket 'unforged_storage' can be manipulated freely without caring about ticket linearity */

  match (unforged_storage) {
    when(Some(x)): do {
      Test.log (["unforged_ticket", x]) ;
      let { ticketer , value , amount } = x ;
      assert (value == ticket_info[0]) ;
      assert (amount == ticket_info[1]) ;
      return unit
    };
    when(None()): failwith ("impossible")
  }
};
```

</Syntax>

result:

```bash
("unforged_ticket" , {amount = 15n ; ticketer = KT1Qp8u3v4seQHPYfpSw6eWvPG8CojH3m18G ; value = 0x0202})
Everything at the top-level was executed.
- test_originate_contract exited with value ().
```
