---
id: tickets
title: Tickets
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

Tezos tickets are authenticated quantities issued by contracts.
A ticket of type `ticket` has three elements:

- Its _ticketer_, which is the contract that issued the ticket

- Its _contents_, also knowns as the wrapped value or payload, which can be any data type

- Its _amount_ of type `nat`, which is an arbitrary number that represents a quantity or value for the ticket

A ticket's ticketer and contents cannot be changed.

Tickets themselves cannot be duplicated, but you can split one ticket
into multiple tickets by creating duplicate tickets each with a
portion of the original ticket's amount.  The new tickets have the
same ticketer and contents, and the sum of their amounts is always the
amount of the original ticket.  Similarly, you can join tickets with
matching ticketers and contents into a single ticket with the sum of
the joined tickets' amounts.

## Creating tickets

To create a ticket, pass the contents and the amount to
`Tezos.Ticket.create` function.  The function returns an
option that contains the ticket or `None` if the amount of the ticket
is zero.  The contract's address automatically becomes the ticketer
value.

<SyntaxTitle syntax="cameligo">
val create : 'value -> nat -> ('value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
create: 'value => nat => option&lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
module Tezos = Tezos.Next
module Ticket = Tezos.Ticket

let my_ticket1 = Option.value_with_error "ticket1 failed" (Ticket.create 1 10n)
let my_ticket2 = Option.value_with_error "ticket2 failed" (Ticket.create "one" 10n)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=manip_ticket
import Tezos = Tezos.Next;
import Ticket = Tezos.Ticket;

const my_ticket1 =
  Option.value_with_error("ticket1 failed", Ticket.create(1, 10 as nat));
const my_ticket2 =
  Option.value_with_error("ticket2 failed", Ticket.create("one", 10 as nat));
```

</Syntax>

## Reading tickets

You cannot read the contents of a ticket directly; you must use the
`Tezos.Ticket.read` function to access it.  This function
destroys the ticket and returns the ticketer, contents, amount, and a
copy of the original ticket.

Note that reading a ticket with the `Tezos.Ticket.read`
function consumes it, destroying the original ticket.  To preserve the
ticket, you must use the copy that the function returns, or else the
ticket is destroyed.

<SyntaxTitle syntax="cameligo">
val read : 'value ticket -> (address * ('value * nat)) * 'value ticket
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
read: ticket&lt;'value&gt; => &lt;&lt;address, &lt;'value , nat&gt;&gt; , ticket&lt;'value&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

To read the content of a ticket, you can either use tuple
destructuring or pattern matching:

```cameligo group=manip_ticket
let v =
  let (_addr, (payload, _amt)), _ticket = Ticket.read my_ticket1
  in payload
```

</Syntax>

<Syntax syntax="jsligo">

To read the content of a ticket, you need to use tuple destructuring:

```jsligo group=manip_ticket
const v2 = (() => {
  let [[_addr, [payload, _amt]], _ticket] = Ticket.read(my_ticket2);
  return payload;
})()
```

</Syntax>

## Splitting tickets

Splitting a ticket creates two tickets that have the same ticketer and
contents as the original and have amounts that add up to the amount of
the original To split a ticket, pass the ticket and two nats to the
`Tezos.Ticket.split` function.  It returns an option that is
`None` if the sum of the two nats does not equal the amount of the
original ticket.  If the sum is equal, it returns `Some` with two
tickets with the two nats as their amounts.

You can split tickets to divide a ticket to send to multiple sources or to consume only part of a ticket's amount.

<SyntaxTitle syntax="cameligo">
val split : 'value ticket -> nat * nat -> ('value ticket * 'value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
split : ticket&lt;'value&gt; => &lt;nat , nat&gt; => option &lt;&lt;ticket&lt;'value&gt;, ticket&lt;'value&gt;&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let ta, tb =
  match Ticket.split my_ticket1 (6n, 4n) with
    None -> failwith "amt_a + amt_v <> amt"
  | Some split_tickets -> split_tickets
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=manip_ticket
const [ta, tb] =
  $match(Ticket.split(my_ticket1, [6 as nat, 4 as nat]), {
    "None": () => failwith("amt_a + amt_v != amt"),
    "Some": split_tickets => split_tickets
  });
```

</Syntax>

## Joining tickets

You can join tickets that have identical ticketers and contents.  The
`Tezos.Ticket.join` function joins tickets and returns an
option with `Some` with a single ticket that has an amount that equals
the sum of the amounts of the original tickets.  If the ticketer or
contents don't match, it returns `None`.

<SyntaxTitle syntax="cameligo">
val join : 'value ticket * 'value ticket -> ('value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
join : &lt;ticket&lt;'value&gt;, ticket&lt;'value&gt;&gt; => option &lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket2
module Tezos = Tezos.Next
module Ticket = Tezos.Ticket

let tc : int ticket option =
  let ta = Option.value_with_error "ta failed" (Ticket.create 1 10n) in
  let tb = Option.value_with_error "tb failed" (Ticket.create 1 5n) in
  Ticket.join (ta, tb)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=manip_ticket2
import Tezos = Tezos.Next;
import Ticket = Tezos.Ticket;

const ta = Option.value_with_error("ta failed",
                                   Ticket.create(1, 10 as nat));
const tb = Option.value_with_error("tb failed",
                                   Ticket.create(1, 5 as nat));
const tc = Ticket.join([ta, tb]);
```

</Syntax>

## Transferring tickets

You can send tickets to other contracts by passing them with the
`Tezos.Operation.transaction` function, just like passing any other
value to a contract.

<!-- updated use of entry -->
