---
id: tickets
title: Tickets
description: Ticket operations for Tezos
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## Tickets

<SyntaxTitle syntax="pascaligo">
val create_ticket&lt;value&gt; : value -> nat -> option (ticket (value))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val create_ticket : 'value -> nat -> ('value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let create_ticket: 'value => nat => option&lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>


To create a ticket, the value and the amount of tickets to be created needs to be provided.
The ticket will also contain the contract address it originated from (which corresponds to `Tezos.self`).
The resulting value is `None` if the amount is zero.

<Syntax syntax="pascaligo">

```pascaligo group=manip_ticket
const my_ticket1 = Option.unopt (Tezos.create_ticket (1, 10n))
const my_ticket2 = Option.unopt (Tezos.create_ticket ("one", 10n))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let my_ticket1 = Option.unopt (Tezos.create_ticket 1 10n)
let my_ticket2 = Option.unopt (Tezos.create_ticket "one" 10n)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=manip_ticket
let my_ticket1 = Option.unopt(Tezos.create_ticket(1, 10n));
let my_ticket2 = Option.unopt(Tezos.create_ticket("one", 10n));
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val read_ticket&lt;value&gt; : ticket (value) -> (address * (value * nat)) * ticket (value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val read_ticket : 'value ticket -> (address * ('value * nat)) * 'value ticket
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let read_ticket: ticket&lt;'value&gt; => &lt;&lt;address, &lt;'value , nat&gt;&gt; , ticket&lt;'value&gt;&gt;
</SyntaxTitle>


Reading a ticket will return a tuple with the ticket address, the value and the same ticket for later use.
A ticket is only consumed when it is dropped (e.g. `DROP`-ed from the Michelson stack) so if the returned ticket isn't stored in some form by your contract, it will be fully consumed.

<Syntax syntax="pascaligo">

To read the content of a ticket, you need to use pattern matching

```pascaligo group=manip_ticket
const v =
  case Tezos.read_ticket (my_ticket1) of [
    ((_addr, (payload, _amt)), _ticket) -> payload
  ]
```

</Syntax>

<Syntax syntax="cameligo">

To read the content of a ticket, you can either use tuple
destructuring or pattern matching:

```cameligo group=manip_ticket
let v =
  let (_addr, (payload, _amt)), _ticket = Tezos.read_ticket my_ticket1
  in payload
```

</Syntax>

<Syntax syntax="jsligo">

To read the content of a ticket, you need to use tuple destructuring:

```jsligo group=manip_ticket
let v2 = (_: unit) => {
  let [[addr, [v, amt]], ticket] = Tezos.read_ticket (my_ticket2);
  return v;
}
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val split_ticket&lt;value&gt; : ticket (value) -> nat * nat -> option (ticket (value) * ticket (value))
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val split_ticket : 'value ticket -> nat * nat -> ('value ticket * 'value ticket) option
</SyntaxTitle>



<SyntaxTitle syntax="jsligo">
let split_ticket: ticket&lt;'value&gt; => &lt;nat , nat&gt; => option &lt;&lt;ticket&lt;'value&gt;, ticket&lt;'value&gt;&gt;&gt;
</SyntaxTitle>

To partially use/consume a ticket, you have to split it.
Provided a ticket and two amounts, two new tickets will be returned to you if, and only if, the sum equals to the amount of the original ticket.

<Syntax syntax="pascaligo">

```pascaligo group=manip_ticket
const x =
  case Tezos.split_ticket (my_ticket1, (6n, 4n)) of [
    None -> failwith ("amt_a + amt_v =/= amt")
  | Some (split_tickets) -> split_tickets
  ]
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let ta, tb =
  match Tezos.split_ticket my_ticket1 (6n, 4n) with
    None -> failwith "amt_a + amt_v <> amt"
  | Some split_tickets -> split_tickets
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=manip_ticket
let [ta, tb] =
  match(Tezos.split_ticket(my_ticket1, [6n, 4n])) {
    when(None()): failwith("amt_a + amt_v != amt");
    when(Some(split_tickets)): split_tickets
  };
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val join_tickets&lt;value&gt; : ticket (value) * ticket (value) -> option (ticket (value))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val join_tickets : 'value ticket * 'value ticket -> ('value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let join_tickets = &lt;ticket&lt;'value&gt;, ticket&lt;'value&gt;&gt; => option &lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>

To add two tickets, you have to join them. This works as the inverse
of `Tezos.split_ticket`.  Provided two tickets with the same ticketer
and content, they are deleted and a new ticket will be returned with
an amount equal to the sum of the amounts of the input tickets.

<Syntax syntax="pascaligo">

```pascaligo group=manip_ticket
const tc = {
  const ta = Option.unopt (Tezos.create_ticket (1, 10n));
  const tb = Option.unopt (Tezos.create_ticket (1, 5n))
} with Tezos.join_tickets ((ta, tb))
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let tc : int ticket option =
  let ta = Option.unopt (Tezos.create_ticket 1 10n) in
  let tb = Option.unopt (Tezos.create_ticket 1 5n) in
  Tezos.join_tickets (ta, tb)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=manip_ticket2
let ta = Option.unopt(Tezos.create_ticket(1, 10n));
let tb = Option.unopt(Tezos.create_ticket(1, 5n));
let tc = Tezos.join_tickets([ta, tb]);
```

</Syntax>
