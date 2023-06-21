---
id: getting-started
title: Getting started
---

This section is aimed at newcomers to Ligo and Tezos smart-contracts.
In this tutorial, we will go through the following step :

- Setting up the development environment,
- Writing a simple contract in Cameligo
- Testing the contract
- Deploying the contract to Tezos

# Setting up the development environment.

At the present moment, we recommend the user to develop on a UNIX system, GNU/Linux or MacOSX as the windows native binary is still in preparation. You can still use Ligo on windows through our docker image
More on [installation](../../intro/installation.md) and [editor support](../../intro/editor-support.md)

Alternatively, you can decide to use our [webide](https://ide.ligolang.org/). This can be useful for testing or for small project. However, it doesn't scale well for bigger size project as you won't be able to spread your project across multiple files and use your own libraries.

## Install ligo

### Static Linux binary

The `ligo` executable is statically linked. It should run on most modern Linux distributions.

You can get the rolling release [here](https://gitlab.com/ligolang/ligo/-/jobs/4515773937/artifacts/raw/ligo), make it executable, and you are done!

```zsh
wget https://gitlab.com/ligolang/ligo/-/jobs/4515773937/artifacts/raw/ligo
chmod +x ./ligo
```

For a specific version, you can visit our [release page](https://gitlab.com/ligolang/ligo/-/releases/).
Optionally, you can put it somewhere in your `PATH` for easy access:

```zsh
sudo cp ./ligo /usr/local/bin
```

### MacOS

Try our tap,

```
brew tap ligolang/ligo https://gitlab.com/ligolang/ligo.git
brew install ligolang/ligo/ligo
```

### Debian Linux package installation

A `.deb` package containing the static `ligo` executable is also available.
First, download [the package](https://gitlab.com/ligolang/ligo/-/jobs/4515773937/artifacts/raw/ligo.deb), and then install using:

```zsh
sudo apt install ./ligo.deb
```

### Dockerised installation

If you've [installed 🐳 Docker](https://docs.docker.com/install/), you can run the latest [LIGO release ](https://ligolang.org/docs/next/intro/changelog):

Linux or OSX:

> ```sh
> docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:
> ```
>
> For convenience you can alias the above command
>
> ```sh
> alias ligo="docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:"
> ```
>
> To make this `alias` persistent across terminal sessions you need to configure your shell.
> Here is a [good link](https://www.tecmint.com/create-alias-in-linux/) with the steps on how to do that.

Windows:

> ```dos
> docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:`
> ```
>
> For convenience you can alias the above command
>
> ```dos
> doskey ligo=docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo: $*
> ```
>
> To make the alias persistent across terminal sessions you need to add the `doskey` to the Windows Registry.
> Follow [this stackoverflow answer](https://stackoverflow.com/a/21040825) for the steps on how to do that.

Or if you want the development version, replace the version above with `next`.

Or run one of the older versions found on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

## Setting up the editor

You can see the updated list of supported editor [here](../../intro/editor-support.md)

In this tutorial, we will use vs-code.

- For vs-code, simply go to the extension menu in the left bar (Ctrl + Shift + X) and search for the `ligo-vscode` extension and install it.

- For emacs, follow the instruction [here](https://gitlab.com/ligolang/ligo/-/blob/dev/tools/emacs/README.md)

- For vim, follow the instruction [here](https://gitlab.com/ligolang/ligo/-/blob/dev/tools/vim/ligo/start/ligo/README.md)

Once, you've done it, you are ready to make your first smart-contract

## Install the Tezos tools

To deploy your smart-contract on the network and to test it, you will need to use a Tezos client.

- On GNU/Linux, the simplest way to get octez-client is through opam using `opam install tezos`. alternatives are available [here](https://tezos.gitlab.io/introduction/howtoget.html)

- On MacOsX, the software is distributed through a [brew](https://brew.sh/) formula with `brew install tezos`.

## Building a smart-contract.

### From a template

Rather you're curious to see how to make NFT or randomness in LIGO, or you want to have an example for a more complex architecture, you can have a look in [this collection of templates](https://packages.ligolang.org/contracts) made by the LIGO team.

### From scratch

In this section and the following one we will use a simple smart-contract that is present as example on our webide. We will cover the LIGO language and smart-contract development in the following tutorials.

First, create a `ligo_tutorial` folder on your computer. Then download and put the contract in this folder. It is available in [CameLIGO](https://gitlab.com/ligolang/ligo/-/raw/dev/src/test/contracts/starting.mligo) and [JsLIGO](https://gitlab.com/ligolang/ligo/-/raw/dev/src/test/contracts/starting.jsligo)

<Syntax syntax="cameligo">

Open your editor in the folder and the file `starting.mligo` in the editor. You should have this code

```cameligo test-ligo group=a
module IncDec = struct
  type storage = int
  type return = operation list * storage

  (* Three entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : return = [], store + delta
  [@entry] let decrement (delta : int) (store : storage) : return = [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : return = [], 0
end
```

</Syntax>

<Syntax syntax="jsligo">

Open your editor in the folder and the file `starting.jsligo` in the editor. You should have this code

```jsligo test-ligo group=a
namespace IncDec {
  type storage = int;
  type ret = [list<operation>, storage];

  // Three entrypoints

  @entry
  const increment = (delta : int, store : storage) : ret => [list([]), store + delta];

  @entry
  const decrement = (delta : int, store : storage) : ret => [list([]), store - delta];

  @entry
  const reset = (_ : unit, _ : storage) : ret => [list([]), 0];
};
```

</Syntax>

Now we are going to compile the contract, open a terminal in the folder. (or the vs-code built-in terminal with Ctrl+shift+²) and run the following command:

<Syntax syntax="cameligo">

```zsh
ligo compile contract starting.mligo -m IncDec -o starting.tz
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile contract starting.jsligo -m IncDec -o starting.tz
```

</Syntax>

The `compile contract` takes a filename, the file you want to
compile. The `-m` parameter indicates the namespace or module
corresponding to the contract to compile inside the file.  The `-o`
parameter indicates to store the result in `starting.tz` instead of
outputting it in the terminal.

LIGO will aggregate all functions marked with `@entry` inside the
module/namespace and build a contract function from them that
dispatches according to the parameter passed. This function is the
default contract function compiled, but if there are no functions
marked with `@entry`, LIGO will look for an entrypoint with name
`main`. Different entrypoints can be selected using `-e`.

Now, you should have a Michelson contract `starting.tz` in the folder
ready to be deploy. But before that, we want to test it to be sure
that it behaves as expected, because once publish, it cannot be
modified.

# Testing the contract

As we can never underline enough the importance of tests in the context of smart-contract. We will now test our contract three times on different levels :

## Test the code from the command line

Using the `interpret` command, one can run LIGO code in the context of an init file. For instance

<Syntax syntax="cameligo">

```zsh
ligo run interpret "<code>" --init-file starting.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run interpret "<code>" --init-file starting.jsligo
```

</Syntax>

will run `<code>` after evaluating everything in the contract file. This is useful to test arbitrary functions and variables in your code.

For instance, to test the `add` function you can run

<Syntax syntax="cameligo">

```zsh
ligo run interpret "IncDec.increment(10,32)" --init-file starting.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run interpret "IncDec.increment(10,32)" --init-file starting.jsligo
```

</Syntax>

which should return `42`.
Running several of this command will cover the complete code.

To run the contract as if it was called on the blockchain, the command
`dry-run` can be used. It takes the contract, the parameter and the
initial storage, and we also need to pass the namespace/module using `-m`:
<Syntax syntax="cameligo">

```zsh
ligo run dry-run starting.mligo "Increment(32)" "10" -m IncDec
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run dry-run starting.jsligo "Increment(32)" "10" -m IncDec
```

</Syntax>

which will return `(LIST_EMPTY(), 42)`.

Combine several of those command to fully test the contract use-cases.

## Test the code with LIGO test framework.

In LIGO, you are able to write tests directly in the source file, using the `Test` module.

<Syntax syntax="cameligo">

Add the following line at the end of `starting.mligo`

```cameligo test-ligo group=a
let test_increment =
  let initial_storage = 10 in
  let (taddr, _, _) = Test.originate_module (contract_of IncDec) initial_storage 0tez in
  let contr = Test.to_contract(taddr) in
  let _ = Test.transfer_to_contract_exn contr (Increment (32)) 1mutez  in
  assert (Test.get_storage(taddr) = initial_storage + 32)
```

</Syntax>

<Syntax syntax="jsligo">

Add the following line at the end of `starting.jsligo`

```jsligo test-ligo group=a
const test_increment = (() : unit => {
  let initial_storage = 10 as int;
  let [taddr, _, _] = Test.originate_module(contract_of(IncDec), initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (1)), 1 as mutez);
  return assert(Test.get_storage(taddr) == initial_storage + 1);
}) ()
```

</Syntax>

which execute the same test as the previous section.

Now simply run the command
<Syntax syntax="cameligo">

```zsh
ligo run test starting.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run test starting.jsligo
```

</Syntax>

The command will run every function starting with `test` and return their values.

More on the syntax for the test framework
[here](https://ligolang.org/docs/advanced/testing#testing-with-test)
and more on how to write and test namespace/module contracts
[here](https://ligolang.org/docs/next/language-basics/modules#modules-as-contracts).

## Testing the Michelson contract

The LIGO compiler is made so the produced Michelson program types and correspond to the initial LIGO program. However until we have tools for formal verification, we advise testing that the Michelson code will behave as the LIGO one. For this purpose, you should also write a test for the Michelson code.

There is different methods for testing Michelson code. In this tutorial we will focus on `octez-client` mockup. More information [here](https://ligolang.org/docs/advanced/michelson_testing)

This method consist in running a "mockup" Tezos chain on our computer, push the contract on the chain and send transaction to the chain to test the contract behaviour.

First, create a temporary folder for the mockup chain by running

```zsh
mkdir /tmp/mockup
```

Now start the node by running

```zsh
octez-client \
  --base-dir /tmp/mockup \
  --mode mockup \
  create mockup
```

This will run the node using the `Lima` protocol and return a few address, aliased from bootstrap1 to 5. For other version, check
`octez-client list mockup protocols`

You can now originate the contract to the mock net with :

```zsh
octez-client \
  --base-dir /tmp/mockup \
  --mode mockup \
  originate contract mockup_testme \
              transferring 0 from bootstrap1 \
              running starting.tz \
              --init 10 --burn-cap 0.1
```

you should see a lot of information on the command line and the information `New contract ... originated`

You can now start testing the contract.

To check its storage run :

```zsh
octez-client \
  --base-dir /tmp/mockup \
  --mode mockup \
  get contract storage for mockup_testme
```

You should see a `10` in your terminal

We are now ready to send a transaction to our contract. We want to send a transaction with parameter "Increment (32)" but the parameter is written is LIGO.
For that, it must first be converted to a Michelson parameter. Which is done by running :

<Syntax syntax="pascaligo">

```zsh
ligo compile parameter starting.ligo "Increment (32)" -m IncDec
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo compile parameter starting.mligo "Increment (32)" -m IncDec
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile parameter starting.jsligo "Increment (32)" -m IncDec
```

</Syntax>

Which gives you the result `(Left (Right 32))`

Now we can send our transaction with the command

```zsh
octez-client \
  --base-dir /tmp/mockup \
  --mode mockup \
transfer 0 from bootstrap2 \
              to mockup_testme \
              --arg "(Left (Right 32))" --burn-cap 0.01
```

The network will again send back many information including the updated storage which should now be equal to 42.

This conclude our section about testing. As an exercise, you can write the test for the other entrypoint (decrease and reset).
Once you are sure that the contract work correctly for all the use cases, you can move on to the next section

# Publishing the contract

For deploying the contract on Tezos, we will use the `octez-client` interface like we did on the previous section.

First, you will need an account address. You can get one using any wallet listed [here](https://tezos.com/learn/store-and-use/).
Once you have your first account configured, go to a [faucet](https://faucet.marigold.dev/), select the `ghostnet` testnet and claim `XTZ` tokens. clikc on the faucet and you will receive some tokens to play with.

Then we are going to point the Tezos client to a Ghostnet testnet node

```zsh
octez-client --endpoint https://ghostnet.tezos.marigold.dev/ config update
```

This is the testnet, which is a separate network from Tezos, use for testing.

Export the mnemonic from your wallet (almost every wallet does it, look on settings or read wallet documentation to see how to do it), then import your account locally. Type on the terminal

```bash
octez-client import keys from mnemonic myWallet
```

Paste the mnemonic when prompt appears

You are now ready to originate your contract with your user. On your wallet, copy your public hash key address `tz1...` or `tz2...` and replace the placeholder `<my_tz_address...>` on the command you need to run :

```zsh
octez-client originate contract increment \
              transferring 0 from <my_tz_address...> \
              running starting.tz \
              --init 10 --burn-cap 0.1 --force
```

Again, you will receive several messages from the node and you should get the confirmation that the contract has been published.

You can search your contract on the network using the portal [Better call dev](https://better-call.dev/)

You can know call your contract with

```zsh
octez-client call increment from <my_tz_address...> \
             --arg "(Left (Right 32))" \
             --burn-cap 0.1
```

If you do so, you will see several information on the operation, including the new contract storage.

This conclude this part of our tutorial.
You should now be able to compile, test, publish and call a contract.
Now you can go to the tacos shop tutorial to know more about programming with LIGO or you can start developing your own contract using the LIGO syntax you are more familiar with.
