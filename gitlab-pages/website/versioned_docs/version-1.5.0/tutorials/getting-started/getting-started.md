---
id: getting-started
title: Getting started
---

This section is aimed at newcomers to Ligo and Tezos smart-contracts.
In this tutorial, we will go through the following steps :

- Writing a simple contract
- Testing the contract
- Deploying the contract to Tezos

## Before to start

Two choices are offered, ideal if you want to work with Ligo :
- Install necessary stuff onto your machine
    - [Ligo compiler](https://ligolang.org/docs/intro/installation) to compile your code.
    - [IDE plugins](https://ligolang.org/docs/intro/editor-support)
    - [octez-client](https://tezos.gitlab.io/introduction/howtoget.html) used to interact with tezos blockchain. Pre-built binaries are available [here](https://github.com/serokell/tezos-packaging)
- Use [webide](https://ide.ligolang.org), ideal if you want a quick view of ligo. You'll be able to do, test, dry-run, and deploy the code !

## Building a smart-contract.

We will use a simple  smart contract in this section and the following one. A counter that is available on a homepage.

First, create a `ligo_tutorial` folder on your computer.  
`mligo` is the extension of cameligo file and `jsligo` for the jsligo file.

<Syntax syntax="cameligo">

Here you are using cameligo syntax so create a file named `main.mligo`, with the following content

```cameligo test-ligo group=a
module Counter = struct
  type storage = int
  type return = operation list * storage

  (* Three entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : return = [], store + delta
  [@entry] let decrement (delta : int) (store : storage) : return = [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : return = [], 0
end
```
### What is a Module ?

A `Module` in Ligo provide a scope to the identifiers (names of types, functions, variables, etc) to prevent collisions between them. Access to it using dot notation :
```
<module>.<identifier>
```
</Syntax>

<Syntax syntax="jsligo">

Here you are using jsligo syntax so create a file named `main.jsligo`, with the following content

```jsligo test-ligo group=a
namespace Counter {
  type storage = int;
  type ret = [list<operation>, storage];

  // Three entrypoints

  @entry
  const increment = (delta : int, store : storage) : ret => [list([]), store + delta];

  @entry
  const decrement = (delta : int, store : storage) : ret => [list([]), store - delta];

  @entry
  const reset = (_p : unit, _s : storage) : ret => [list([]), 0];
};
```

### What is a namespace ?

A namespace is the jsligo keyword to declare a Module in Ligo it provides a scope to the identifiers (names of types, functions, variables, etc) to prevent collisions between them. Only exported identifiers can be accessed from the outside using dot notation. More details in [Module section](https://ligolang.org/docs/language-basics/modules?lang=jsligo)
```
<namespace>.<identifier>
```

</Syntax>

### What is a storage ?

Storage is the datas stored in your smart-contract, in michelson and on-chain. When you deploy your contract you will pay for the storage, so keep it as light as possible and use it only for data which has to be in blockchain. See [Optimisation section](https://ligolang.org/docs/tutorials/optimisation/) for more details.

When you will need to deploy a contract, the storage will need an init value (flag `--init` onto `octez-client`) defined in `Michelson`. It's possible to express it in Ligo using [compile storage command](https://ligolang.org/docs/manpages/compile%20storage).

```
ligo compile storage <your_main_file> <ligo_expression> -m <your_module_in_main_file>
```


### What is an entry ?

An [entry-point](https://ligolang.org/docs/advanced/entrypoints-contracts?lang=jsligo#entry-points) is a pure function marked with `@entry` annotation that can be formalized as `[list<operation>, storage] = f(parameter, storage)` where :
- f is the entry-point function that is called
- parameter contains the inputs that are sent by the caller when the contract is called
- storage is the state of the storage. The input is the current state of the storage, and it outputs the next state
- `list<operation>` is a list of commands that will be executed by the block chain (for instance transfers)

It's also a function that the RPC node of the Tezos blockchain will expose. It'll be possible to interact with your smart-contract by calling them.

# Compile a contract

Now we are going to compile the contract
If you are using the Web-ide ensure to target the module Counter in `config.json` file and click on Compile, This will allow you to see the produced Michelson.

On your machine we will use the ligo compiler CLI, open a terminal in the folder. (or the vs-code built-in terminal with Ctrl+shift+²) and run the following command:

<Syntax syntax="cameligo">

```zsh
ligo compile contract main.mligo -m Counter -o counter.tz
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile contract main.jsligo -m Counter -o counter.tz
```

</Syntax>

The `compile contract` takes a filename, the file you want to
compile. The `-m` parameter indicates the namespace or module
corresponding to the contract to compile inside the file.  The `-o`
parameter indicates to store the result in `counter.tz` instead of
outputting it in the terminal.

LIGO will aggregate all functions marked with `@entry` inside the
module/namespace and build a contract function from them that
dispatches according to the parameter passed. This function is the
default contract function compiled (`$main`).

Now, you should have a Michelson contract `counter.tz` in the folder
ready to be deploy. But before that, we want to test it to be sure
that it behaves as expected because once publish, it cannot be
modified.

# Testing the contract

Because we can never underline enough the importance of tests in the context of smart contract; we will now test our contract three times on different levels :

## Test the code from the command line

Using the `interpret` command, one can run LIGO code in the context of an init file. For instance

<Syntax syntax="cameligo">

```zsh
ligo run interpret "<code>" --init-file main.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run interpret "<code>" --init-file main.jsligo
```

</Syntax>

will run `<code>` after evaluating everything in the contract file. This is useful to test arbitrary functions and variables in your code.

For instance, to test the `add` function you can run

<Syntax syntax="cameligo">

```zsh
ligo run interpret "Counter.increment 10 32" --init-file main.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run interpret "Counter.increment(10,32)" --init-file main.jsligo
```

</Syntax>

which should return `42`.
Running several of this command will cover the complete code.

To run the contract **as if it was called on the blockchain**, the command
`dry-run` can be used. It takes the contract, the parameter and the
initial storage, and we also need to pass the namespace/module using `-m`:
<Syntax syntax="cameligo">

```zsh
ligo run dry-run main.mligo "Increment(32)" "10" -m Counter
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run dry-run main.jsligo "Increment(32)" "10" -m Counter
```

</Syntax>

which will return `(LIST_EMPTY(), 42)`.

Combine several of those command to fully test the contract use-cases.

## Test the code with LIGO test framework.

In LIGO, you are able to write tests directly in the source file, using the `Test` module. We generally encourage writing tests in a separate file, but we will use the same file for this example.

<Syntax syntax="cameligo">

Add the following line at the end of `main.mligo`

```cameligo test-ligo group=a
let test_increment =
  let initial_storage = 10 in
  let orig = Test.originate (contract_of Counter) initial_storage 0tez in
  let _ = Test.transfer_exn orig.addr (Increment (32)) 1mutez in
  assert (Test.get_storage(orig.addr) = initial_storage + 32)
```

</Syntax>

<Syntax syntax="jsligo">

Add the following line at the end of `main.jsligo`

```jsligo test-ligo group=a
const test_increment = (() : unit => {
  let initial_storage = 10 as int;
  let orig = Test.originate(contract_of(Counter), initial_storage, 0tez);
  Test.transfer_exn(orig.addr, (Increment (1)), 1mutez);
  return assert(Test.get_storage(orig.addr) == initial_storage + 1);
}) ()
```

</Syntax>

This executes the same test as the previous section.

Now simply run the command
<Syntax syntax="cameligo">

```zsh
ligo run test main.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run test main.jsligo
```

</Syntax>

The command will run every function main with `test` and return their values.

More on the syntax for the test framework
[here](https://ligolang.org/docs/advanced/testing#testing-with-test)
and more on how to write and test namespace/module contracts
[here](https://ligolang.org/docs/next/language-basics/modules#modules-as-contracts).

# Publishing the contract

For deploying the contract on Tezos, we will use the `octez-client` interface as we did on the previous section.

First, you will need an account address. You can get one using any wallet listed [here](https://tezos.com/learn/store-and-use/).
Once you have your first account configured, go to a [faucet](https://faucet.marigold.dev/), select the `ghostnet` testnet and claim `XTZ` tokens. clikc on the faucet and you will receive some tokens to play with.

Then we are going to point the Tezos client to a Ghostnet testnet node

```zsh
octez-client --endpoint https://ghostnet.tezos.marigold.dev/ config update
```

Ghostnet is a testnet, which is a separate network from the Tezos mainnet, which can be used for testing.

Export the mnemonic from your wallet (almost every wallet does it, look on settings or read wallet documentation to see how to do it), then import your account locally. Type on the terminal

```bash
octez-client import keys from mnemonic myWallet
```

Paste the mnemonic when prompt appears

You are now ready to originate your contract with your user. On your wallet, copy your public hash key address `tz1...` or `tz2...` and replace the placeholder `<my_tz_address...>` on the command you need to run :

```zsh
octez-client originate contract counter \
              transferring 0 from <my_tz_address...> \
              running counter.tz \
              --init 10 --burn-cap 0.1 --force
```

Again, you will receive several messages from the node and you should get the confirmation that the contract has been published. Note the `KT1...` address available in logs, you'll be able to find your contract onto an indexer like `tzkt` through url like `https://ghostnet.tzkt.io/KT1.../` don't forget to put your `KT1` address

You can know call your contract with

```zsh
octez-client call counter from <my_tz_address...> \
             --arg "(Left (Right 32))" \
             --burn-cap 0.1
```


arg is obtained by compiling ligo expression onto michelson 

<Syntax syntax="cameligo">

```zsh
ligo compile parameter main.mligo "Increment (32)" -m Counter
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile parameter main.jsligo "Increment (32)" -m Counter
```

</Syntax>

If you do so, back to `tzkt`, you will see several information on the operation, including the new contract storage.


## Testing the Michelson contract locally

It can be annoying to deploy you contract onto a node to test it. We advise testing that [the Michelson code locally using mockup environment](https://tezos.gitlab.io/user/mockup.html). It'll allow to automate end to end tests in simulated environment

This conclude this part of our tutorial.
You should now be able to compile, test, publish and call a contract.
Now you can go to the [tacos shop tutorial](https://ligolang.org/docs/tutorials/taco-shop/tezos-taco-shop-smart-contract) to know more about programming with LIGO or you can start developing your own contract using the LIGO syntax you are more familiar with.
