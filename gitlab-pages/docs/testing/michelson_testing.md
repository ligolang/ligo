---
id: michelson_testing
title: Testing Michelson code
---

import Syntax from '@theme/Syntax';

After you test and compile your LIGO contracts, you may want to test the compiled Michelson output.
There are two frameworks for testing Michelson contracts:

* [PyTezos](https://baking-bad.org/blog/2019/09/16/testing-michelson-tezos-contracts-with-pytezos-library/)

* [Cleveland](https://gitlab.com/morley-framework/morley/-/blob/9455cd384b2ab897fb7b31822abca3730a4ad08b/code/cleveland/testingEDSL.md)

## Testing in sandboxes and testnets

You can also test compiled contracts in sandboxes and on test networks, which work in a way similar to Tezos Mainnet but do not have the same costs.
Several options for sandboxes and testnets are available, including:

- The Octez suite [mockup mode](https://tezos.gitlab.io/user/mockup.html) and [sandbox mode](https://tezos.gitlab.io/user/sandbox.html)
- Local test networks such as [Flextesa](https://tezos.gitlab.io/flextesa/) and [Tezbox](https://github.com/tez-capital/tezbox)
- Public test networks such as Ghostnet, which are listed at https://teztnets.com

For more information about the options, see [Testing on sandboxes and testnets](https://docs.tezos.com/developing/testnets) on docs.tezos.com.

## Testing with the Octez client mockup mode

One way to test compiled Michelson contracts is to use the mockup mode of the Octez client, which runs a simulation of Tezos without using nodes.

The first step is to compile the LIGO contract to Michelson.
For example, here is a simple contract that stores a string and allows users to add to that string or reset it:

<Syntax syntax="cameligo">

```cameligo group=mockup_testme
(* This is mockup_testme.mligo *)
type storage = string

type result = operation list * storage

[@entry]
let append (s : string) (storage : storage) : result =
  [], storage ^ s

[@entry]
let reset (_ : unit) (_storage : storage) : result =
  [], ""
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=mockup_testme
// This is mockup_testme.jsligo
type storage = string;

type result = [list<operation>, storage];

@entry
const append = (s: string, storage: storage): result =>
  [[], storage + s]

@entry
const reset = (_: unit, _storage: storage): result =>
  [[], ""]
```

</Syntax>

To compile it to Michelson, use the `ligo compile contract` command:

<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/testing/src/michelson_testing/mockup_testme.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract gitlab-pages/docs/testing/src/michelson_testing/mockup_testme.jsligo
# Outputs:
```

</Syntax>

The output is the compiled code:

```michelson
{ parameter (or (unit %reset) (string %append)) ;
  storage string ;
  code { UNPAIR ;
         IF_LEFT { DROP 2 ; PUSH string "" } { SWAP ; CONCAT } ;
         NIL operation ;
         PAIR } }
```

To write the compiled code to a file, pass the `--output-file argument`.
This command writes the compiled code to a file named `mockup_testme.tz`:

<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/testing/src/michelson_testing/mockup_testme.mligo --output-file mockup_testme.tz
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract gitlab-pages/docs/testing/src/michelson_testing/mockup_testme.jsligo --output-file mockup_testme.tz
```

</Syntax>

Now you can follow these steps to deploy the compiled contract to the Octez client mockup mode and verify that it works:

1. Install the Octez client.

   One way is to install it via opam by running `opam install octez-client`.
   For other installation methods, see [Installing Octez](https://tezos.gitlab.io/introduction/howtoget.html) in the Octez documentation.

1. Print a list of supported protocol versions by running `octez-client list mockup protocols`.

   The response starts with the Alpha protocol, which is the new version of the protocol that is under development in the `master` branch of the Octez source code repository.
   The next protocol in the list is the most recently released version of the protocol.
   In this case the most recent version is the Paris C protocol, which has the hash `PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi`.

1. Initialize the mockup with the protocol hash and a folder to store the mockup instance in by running this command:

   ```shell skip
   tezos-client \
     --protocol PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi \
     --base-dir /tmp/mockup \
     --mode mockup \
     create mockup
   ```

   This command initializes a mockup in the specified folder and returns a list of Tezos addresses that the client can use in the mockup.

1. Optional: Set up an alias so future Octez client commands use this mockup instead of another Tezos network:

   ```shell
   alias mockup-client='octez-client --mode mockup --base-dir /tmp/mockup'
   ```

   Now you can run the command `mockup-client` to run Octez client transactions in the sandbox and retain the usual `octez-client` command for transactions on public networks.

   For example, you can list the addresses in the mockup again by running this command:

   ```shell skip
   mockup-client list known addresses
   ```

   The response is a list of accounts that have tez on the mockup network and that you can use to send transactions:

   ```
   bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
   bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
   bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
   bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
   bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)
   ```

1. Deploy (originate) the contract on the mockup network by running this command:

   ```shell skip
   mockup-client originate contract mockup_testme \
                 transferring 0 from bootstrap1 \
                 running "`cat mockup_testme.tz`" \
                 --init \"foo\" --burn-cap 0.1
   ```

   The `--init` argument (`"foo"`) is the initial storage for the deployed contract in Michelson format.
   If the storage is more complex, you can use the `ligo compile storage` command to to compile a LIGO expression to a Michelson value.


   If the origination is successful, the client prints the address of the new contract to the console and stores that address with the alias `mockup_testme` from the command.

1. Verify the current value of the contract storage by running this command:

   ```shell
   mockup-client get contract storage for mockup_testme
   ```

   The response is `"foo"`, which matches the value that you set in the origination command.

1. Test the contract by sending a transaction to it:

   1. Compile the transaction parameter to Michelson with the `ligo compile parameter` command.
   For example, this command compiles the parameter to call the the `Append` entrypoint and pass the string `bar`:

      <Syntax syntax="cameligo">

      ```shell
      ligo compile parameter gitlab-pages/docs/testing/src/michelson_testing/mockup_testme.mligo "Append (\"bar\")"
      ```

      </Syntax>

      <Syntax syntax="jsligo">

      ```shell
      ligo compile parameter gitlab-pages/docs/testing/src/michelson_testing/mockup_testme.jsligo "Append (\"bar\")"
      ```

      </Syntax>

      The compiled parameter is `(Right "bar")`, including the double quotes.
      For more information about why this parameter appears like it does, see [Entrypoints](https://docs.tezos.com/smart-contracts/entrypoints) on docs.tezos.com.

   1. Call the contract by passing that parameter value to the Octez client `transfer` command, as in this example:

      ```shell
      mockup-client transfer 0 from bootstrap2 \
          to mockup_testme \
          --arg "(Right \"bar\")" --burn-cap 0.01
      ```

   1. Verify that the storage changed by running the `get contract storage` command again:

      ```shell
      mockup-client get contract storage for mockup_testme
      ```

      The storage now contains the two strings.

In this way you can deploy contracts and test their interaction in a simulation of a Tezos network.
Testing in sandboxes and test networks in this way can be useful to confirm that multiple contracts interact with each other correctly and to confirm that the contract works the way it does in your LIGO tests.

<!-- updated use of entry -->
