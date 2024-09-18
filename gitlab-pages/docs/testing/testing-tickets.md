---
id: testing-tickets
title: Testing tickets
---

Testing code that uses tickets requires extra steps because of how tickets are used in Tezos operations.
LIGO provides the `Proxy_ticket` module to help you test with tickets.

## The problem with testing tickets

Tickets have specific limitations on the Tezos platform that affect how they can be used in LIGO tests.
For example, tickets always have the address of the contract that created it as the ticketer field.
Also, their data payload cannot be changed after the ticket is created.

As a result, you can't create a ticket in a LIGO test with `Tezos.Next.Ticket.create` and use it to test smart contract origination or entrypoints.
If LIGO allowed you to create and use tickets in this way, you could edit the ticket or assign a ticketer that was not the contract that created it.
If you try to use such a ticket in smart contract operations, the operations fail.

You also can't compile LIGO expressions that include tickets and use the compiled Michelson.
If you compile an expression that includes a ticket, the value is represented in Michelson as pairs.
For example, this code compiles a ticket with a binary value as its payload:

<Syntax syntax="cameligo">

```shell
ligo compile expression cameligo 'Tezos.create_ticket 0x0202 10n'
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile expression jsligo 'Tezos.create_ticket(0x0202, 10n)'
```

</Syntax>


The result (within an option type) is represented in pairs, showing the address of the ticketer, the payload, and the amount of the ticket:

```
(Some (Pair "KT1DUMMYDUMMYDUMMYDUMMYDUMMYDUMu2oHG" 0x0202 10))
```

If you pass this value for the parameter of an operation that requires a ticket, the Tezos protocol blocks the operation and does not recognize this value as a ticket because doing so could allow you edit and submit a false ticket.

LIGO testing tools to provide ways to create tickets via a proxy contract so you can use tickets in tests.

## Proxy ticket contracts

The LIGO test library provides a `Proxy_ticket` module which helps in working with tickets in the test framework.
Instead of creating tickets yourself, you use a proxy contract to create tickets and send them with operations.

The `Proxy_ticket` module provides these functions:

- `init_transfer`: Creates a proxy contract that you can use as the source of tickets in test operations
- `transfer`: Uses a proxy contract to create a ticket and send it as the parameter of a smart contract call
- `originate`: Uses a proxy contract to originate a smart contract with initial storage that includes a ticket

:::info
Due to a limitation in the testing framework, you can use the proxy ticket contract only with contracts that contain a single entrypoint.
:::

## Originating contracts with tickets

To originate a contract with one or more tickets in its storage, you can use a proxy contract to generate the tickets and include them in the origination operation.

For example, this contract stores an integer and a ticket.
It provides an entrypoint that reads the ticket and adds its amount to the integer in storage:

<Syntax syntax="cameligo">

```cameligo group=usage_orig
module MyContract = struct

  type storage = int * (bytes ticket) option
  type unforged_storage = int * (bytes unforged_ticket) option

  [@entry] let main (_ : unit) (storage : storage) :operation list * storage =
    let (stored_value, ticket_opt) = storage in
    let new_storage : storage =
      match ticket_opt with
        // If there is a ticket, add its amount to the int in storage
        Some ticket ->
          let ((_address, (_payload, amount)), new_ticket) = Tezos.Next.Ticket.read ticket in
          (stored_value + (int amount), Some new_ticket)
        // If there is no ticket in storage, do nothing
        | None -> stored_value, None ()
      in
    [], new_storage

end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=usage_orig
namespace MyContract {
  export type storage = [int, option<ticket<bytes>>];
  export type unforged_storage = [int, option<unforged_ticket<bytes>>];

  @entry
  const main = (_: unit, storage: storage): [list<operation>, storage] => {
    const [stored_value, ticket_opt] = storage;
    const new_storage: storage =
      match(ticket_opt) {
        // If there is a ticket, add its amount to the int in storage
        when(Some(ticket)): ((ticket: ticket<bytes>) => {
          const [[_address, [_payload, amount]], new_ticket] = Tezos.Next.Ticket.read(ticket);
          return [stored_value + int(amount), Some(new_ticket)];
        })(ticket);
        // If there is no ticket in storage, do nothing
        when(None()): [stored_value, None()];
      };
    return [list([]), new_storage];
  };

}
```

</Syntax>

To originate this contract, you need a ticket for its initial storage value.
To create this ticket, you use a proxy ticket contract.

To create a proxy ticket contract, create a function that returns a value of the initial storage for the contract or the parameter for the smart contract call.
In this example, the function returns zero for the integer and a ticket created by the proxy.
Then, the test uses the proxy contract to originate the contract to test:

<Syntax syntax="cameligo">

```cameligo group=usage_orig
let test_originate_contract =

  // Create a function that the proxy runs to return the contract storage
  let create_storage = fun (t : bytes ticket) : MyContract.storage -> (0, Some t) in
  let ticket_bytes : bytes = 0x0202 in
  let ticket_amount = 15n in
  let ticket_info = ticket_bytes, ticket_amount in

  // Create the proxy contract and use it to originate the contract
  let addr = Test.Proxy_ticket.originate ticket_info create_storage MyContract.main in
  // ...
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=usage_orig
const test_originate_contract = do {

  // Create a function that the proxy runs to return the contract storage
  const create_storage = (t: ticket<bytes>): MyContract.storage => [0, Some (t)];
  const ticket_info = [0x0202, 15n];

  // Create the proxy contract and use it to originate the contract
  const addr = Test.Proxy_ticket.originate (ticket_info, create_storage, MyContract.main);
  // ...
```

</Syntax>

To verify that the ticket is in the contract storage, you must use the `Test.Proxy_ticket.get_storage` function to retrieve the ticket from the contract storage.
This function provides tickets as _unforged tickets_, which are tickets that you can read freely without destroying them and recreating them with the `Tezos.Next.Ticket.read` function.
In this code, the test retrieves the ticket from the contract and verifies its contents:

<Syntax syntax="cameligo">

```cameligo group=usage_orig
  // The ticket 'unforged_ticket_opt' can be manipulated freely without being destroyed
  let unforged_storage : MyContract.unforged_storage = Test.Proxy_ticket.get_storage addr in
  let (_stored_value, unforged_ticket_opt) = unforged_storage in

  // Verify that the ticket is in storage
  let () = match unforged_ticket_opt with
    Some unforged_ticket ->
      let () = Test.Next.IO.log ("unforged_ticket",unforged_ticket) in
      let { ticketer ; value ; amount } = unforged_ticket in
      let () = Assert.assert (value = ticket_bytes) in
      Assert.assert (amount = ticket_amount)
    | None -> failwith "impossible"
    in

  // Call the entrypoint and verify that the value in storage changes
  let _ : nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint"default" addr) unit 0tez in
  let new_storage : MyContract.unforged_storage = Test.Proxy_ticket.get_storage addr in
  let (new_stored_value, _unforged_ticket_opt) = new_storage in
  Assert.assert (new_stored_value = (int ticket_amount))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=usage_orig
  // The ticket 'unforged_ticket_opt' can be manipulated freely without being destroyed
  const [_stored_value, unforged_ticket_opt] = (Test.Proxy_ticket.get_storage(addr) as MyContract.unforged_storage);

  // Verify that the ticket is in storage
  match (unforged_ticket_opt) {
    when(Some(x)): do {
      Test.Next.IO.log(["unforged_ticket", x]);
      const { ticketer: _, value, amount } = x;
      Assert.assert(value == ticket_info[0]);
      Assert.assert(amount == ticket_info[1]);
      return unit
    };
    when(None()): failwith ("impossible")
  };

  // Call the entrypoint and verify that the value in storage changes
  Test.Next.Contract.transfer_exn(Test.Next.Typed_address.get_entrypoint("default", addr), unit, 0tez);
  const [new_stored_value, _unforged_ticket_opt] = Test.Proxy_ticket.get_storage(addr) as MyContract.unforged_storage;
  Assert.assert(new_stored_value == int(ticket_info[1]));
};
```

Note that because this is a single-entrypoint contract, the LIGO compiler renames the entrypoint to `default`.

</Syntax>

## Calling entrypoints with tickets

To test entrypoints that accept tickets, you use a proxy contract to create the ticket and send the call to the entrypoint.
The process is similar to originating a contract with a proxy: you create a function that returns the parameter for the entrypoint and create a proxy based on that function.

For example, this contract has an entrypoint that receives an integer and a ticket that contains an integer.
It multiplies the integer in the first parameter with the integer in the ticket payload and with the ticket amount and adds the result to the integer in storage:

<Syntax syntax="cameligo">

```cameligo group=usage_entrypoint
module MyContract = struct
  type storage = int
  type param = int * int ticket

  [@entry] let main (param : param) (storage : storage) : operation list * storage =
    let (multiplier, ticket) = param in
    // Read the ticket, destroy it, and add its amount times the multiplier to storage
    let ((_address, (payload, amount)), _ticket) = Tezos.Next.Ticket.read ticket in
    [], (storage + (multiplier * payload * (int amount)))
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=usage_entrypoint
namespace MyContract {
  type storage = int;
  export type param = [int, ticket<int>];

  @entry
  function main (param: param, storage: storage): [list<operation>, storage] {
    const [multiplier, ticket] = param;
    // Read the ticket, destroy it, and add its amount times the multiplier to storage
    const [[_address, [payload, amount]], _ticket] = Tezos.Next.Ticket.read(ticket);
    return ([[], storage + (multiplier * payload * int(amount))]);
  };
}
```

</Syntax>

To test the contract, originate it as usual.
Then create a function that returns the parameter for the entrypoint, create a proxy based on this function, and use the proxy to call the entrypoint:

<Syntax syntax="cameligo">

```cameligo group=usage_entrypoint
let test_transfer_to_contract =
  // Originate the contract as usual
  let orig = Test.Next.Originate.contract (contract_of MyContract) 0 0tez in
  let main_addr = Test.Next.Typed_address.to_address orig.taddr in

  // Create a function that the proxy runs to return the parameter
  let create_param : int ticket -> MyContract.param = fun (t : int ticket) -> 5, t in

  // Create the proxy contract
  let proxy_taddr = Test.Proxy_ticket.init_transfer create_param in
  let () = Test.Next.IO.log ("proxy addr:", proxy_taddr) in

  // Use the proxy to call the entrypoint
  let ticket_info = 3, 10n in
  let _ : test_exec_result = Test.Proxy_ticket.transfer proxy_taddr (ticket_info, main_addr) in
  Assert.assert ((Test.Next.Typed_address.get_storage orig.taddr) = 150)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=usage_entrypoint
const test_transfer_to_contract = do {
  // Originate the contract as usual
  let orig = Test.Next.Originate.contract(contract_of(MyContract), 0, 0tez);
  let main_addr = Test.Next.Typed_address.to_address(orig.taddr);

  // Create a function that the proxy runs to return the parameter
  const create_param = (t: ticket<int>): MyContract.param => [5, t];

  // Create the proxy contract
  const proxy_taddr = Test.Proxy_ticket.init_transfer(create_param);
  Test.Next.IO.log(["proxy addr:", proxy_taddr]);

  // Use the proxy to call the entrypoint
  const ticket_info = [3, 10n];
  Test.Proxy_ticket.transfer(proxy_taddr, [ticket_info, main_addr]);

  // Verify that the value in storage changes
  Assert.assert(Test.Next.Typed_address.get_storage(orig.taddr) == 150);
};
```

</Syntax>
