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

You can get the rolling release [here](https://gitlab.com/ligolang/ligo/-/jobs/3625997367/artifacts/raw/ligo), make it executable, and you are done!

```zsh
wget https://gitlab.com/ligolang/ligo/-/jobs/3625997367/artifacts/raw/ligo
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
First, download [the package](https://gitlab.com/ligolang/ligo/-/jobs/3625997367/artifacts/raw/ligo.deb), and then install using:

```zsh
sudo apt install ./ligo.deb
```

### Dockerised installation

If you've [installed 🐳 Docker](https://docs.docker.com/install/), you can run the latest [LIGO release 0.60.0](https://ligolang.org/docs/next/intro/changelog):

Linux or OSX:

> ```sh
> docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.60.0
> ```
>
> For convenience you can alias the above command
>
> ```sh
> alias ligo="docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.60.0"
> ```
>
> To make this `alias` persistent across terminal sessions you need to configure your shell.  
> Here is a [good link](https://www.tecmint.com/create-alias-in-linux/) with the steps on how to do that.

Windows:

> ```dos
> docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:0.60.0`
> ```
>
> For convenience you can alias the above command
>
> ```dos
> doskey ligo=docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:0.60.0 $*
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

- On GNU/Linux, the simplest way to get tezos-client is through opam using `opam install tezos`. alternatives are available [here](https://tezos.gitlab.io/introduction/howtoget.html)

- On MacOsX, the software is distributed through a [brew](https://brew.sh/) formula with `brew install tezos`.

## Building a smart-contract.

### From a template

Rather you're curious to see how to make NFT or randomness in LIGO, or you want to have an example for a more complex architecture, you can have a look in [this collection of templates](https://packages.ligolang.org/templates) made by the LIGO team.

### From scratch

In this section and the following one we will use a simple smart-contract that is present as example on our webide. We will cover the ligo language and smart-contract development in the following tutorials.

First, create a `ligo_tutorial` folder on your computer. Then download and put the contract in this folder. It is available in [Pascaligo](https://gitlab.com/ligolang/ligo/-/raw/dev/src/test/contracts/increment.ligo), [Cameligo](https://gitlab.com/ligolang/ligo/-/raw/dev/src/test/contracts/increment.mligo) and [Jsligo](https://gitlab.com/ligolang/ligo/-/raw/dev/src/test/contracts/increment.jsligo)

<Syntax syntax="pascaligo">

Open your editor in the folder and the file `increment.ligo` in the editor. You should have this code

```pascaligo test-ligo group=a
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

// Two entrypoints

function add (const store : storage; const delta : int) is
  store + delta

function sub (const store : storage; const delta : int) is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : list(operation) * storage is
 (nil,    // No operations
  case action of [
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  ])
```

</Syntax>
<Syntax syntax="cameligo">

Open your editor in the folder and the file `increment.mligo` in the editor. You should have this code

```cameligo test-ligo group=a
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

// Two entrypoints

let add (store, delta : storage * int) = store + delta
let sub (store, delta : storage * int) = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : operation list * storage =
 [],    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

</Syntax>

<Syntax syntax="jsligo">

Open your editor in the folder and the file `increment.jsligo` in the editor. You should have this code

```jsligo test-ligo group=a
type storage = int;

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

/* Two entrypoints */
const add = ([store, delta] : [storage, int]) => store + delta;
const sub = ([store, delta] : [storage, int]) => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
const main = ([action, store] : [parameter, storage]) : [list <operation>, storage] => {
 return [
   list([]),    // No operations
   (match (action, {
    Increment: n => add ([store, n]),
    Decrement: n => sub ([store, n]),
    Reset:     ()  => 0}))
  ]
};
```

</Syntax>

Now we are going to compile the contract, open a terminal in the folder. (or the vs-code built-in terminal with Ctrl+shift+²) and run the following command:

<Syntax syntax="pascaligo">

```zsh
ligo compile contract increment.ligo -o increment.tz
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo compile contract increment.mligo -o increment.tz
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile contract increment.jsligo -o increment.tz
```

</Syntax>

The `compile contract` take one parameters, the file you want to compile. The `-o` parameter indicates to store the result in increment.tz instead of outputting it in the terminal. By default, the `main` function will be use as entrypoint. To select another entrypoint use `-e`

Now, you should have a Michelson contract `increment.tz` in the folder ready to be deploy. But before that, we want to test it to be sure that it behaves as expected, because once publish, it cannot be modified.

## Testing the contract

As we can never underline enough the importance of tests in the context of smart-contract. We will now test our contract three times on different levels :

### Test the code from the command line

Using the `interpret` command, one can run ligo code in the context of an init file. For instance

<Syntax syntax="pascaligo">

```zsh
ligo run interpret "<code>" --init-file increment.ligo
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo run interpret "<code>" --init-file increment.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run interpret "<code>" --init-file increment.jsligo
```

</Syntax>

will run `<code>` after evaluating everything in the contract file. This is useful to test arbitrary functions and variables in your code.

For instance, to test the `add` function you can run
<Syntax syntax="pascaligo">

```zsh
ligo run interpret "add(10,32)" --init-file increment.ligo
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo run interpret "add(10,32)" --init-file increment.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run interpret "add(10,32)" --init-file increment.jsligo
```

</Syntax>

which should return `42`.
Running several of this command will cover the complete code.

To run the contract as called on the blockchain, you will prefer the command `dry-run` which take the contract, the entrypoint, the initial parameter and the initial storage, like so
<Syntax syntax="pascaligo">

```zsh
ligo run dry-run increment.ligo "Increment(32)" "10"
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo run dry-run increment.mligo "Increment(32)" "10"
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run dry-run increment.jsligo "Increment(32)" "10"
```

</Syntax>

which will return `(LIST_EMPTY(), 42)`.

Combine several of those command to fully test the contract use-cases.

### Test the code with ligo test framework.

In LIGO, you are able to write tests directly in the source file, using the `Test` module.

<Syntax syntax="pascaligo">

Add the following line at the end of `increment.ligo`

```pascaligo test-ligo group=a
const test_increment = {
    const initial_storage = 10;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(1), 1mutez);
    const storage = Test.get_storage(taddr);
  } with assert (storage = initial_storage + 1);
```

</Syntax>
<Syntax syntax="cameligo">

Add the following line at the end of `increment.mligo`

```cameligo test-ligo group=a
let test_increment =
  let initial_storage = 10 in
  let (taddr, _, _) = Test.originate main  initial_storage 0tez in
  let contr = Test.to_contract(taddr) in
  let _ = Test.transfer_to_contract_exn contr (Increment (32)) 1mutez  in
  assert (Test.get_storage(taddr) = initial_storage + 32)
```

</Syntax>

<Syntax syntax="jsligo">

Add the following line at the end of `increment.jsligo`

```jsligo test-ligo group=a
const test_increment = (() : unit => {
  let initial_storage = 10 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (1)), 1 as mutez);
  return assert(Test.get_storage(taddr) == initial_storage + 1);
}) ()
```

</Syntax>

which execute the same test as the previous section.

Now simply run the command
<Syntax syntax="pascaligo">

```zsh
ligo run test increment.ligo
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo run test increment.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run test increment.jsligo
```

</Syntax>

The command will run every function starting with `test` and return their values.

More on the syntax for the test framework [here](https://ligolang.org/docs/advanced/testing#testing-with-test).

### Testing the Michelson contract

The ligo compiler is made so the produced Michelson program types and correspond to the initial ligo program. However until we have tools for formal verification, we advise testing that the Michelson code will behave as the ligo one. For this purpose, you should also write a test for the Michelson code.

There is different methods for testing Michelson code. In this tutorial we will focus on `tezos-client` mockup. More information [here](https://ligolang.org/docs/advanced/michelson_testing)

This method consist in running a "mockup" Tezos chain on our computer, push the contract on the chain and send transaction to the chain to test the contract behaviour.

First, create a temporary folder for the mockup chain by running

```zsh
mkdir /tmp/mockup
```

Now start the node by running

```zsh
tezos-client \
  --protocol PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq \
  --base-dir /tmp/mockup \
  --mode mockup \
  create mockup
```

This will run the node using the `Edo` protocol and return a few address, aliased from bootstrap1 to 5. For other version, check
`tezos-client list mockup protocols`

You can now originate the contract to the mock net with :

```zsh
tezos-client \
  --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
  --base-dir /tmp/mockup \
  --mode mockup \
  originate contract mockup_testme \
              transferring 0 from bootstrap1 \
              running increment.tz \
              --init 10 --burn-cap 0.1
```

you should see a lot of information on the command line and the information `New contract ... originated`

You can now start testing the contract.

To check its storage run :

```zsh
tezos-client \
  --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
  --base-dir /tmp/mockup \
  --mode mockup \
  get contract storage for mockup_testme
```

You should see a `10` in your terminal

We are now ready to send a transaction to our contract. We want to send a transaction with parameter "Increment (32)" but the parameter is written is ligo.
For that, it must first be converted to a Michelson parameter. Which is done by running :

<Syntax syntax="pascaligo">

```zsh
ligo compile parameter increment.ligo "Increment (32)"
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo compile parameter increment.mligo "Increment (32)"
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile parameter increment.jsligo "Increment (32)"
```

</Syntax>

Which gives you the result `(Left (Right 32))`

Now we can send our transaction with the command

```zsh
tezos-client \
  --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
  --base-dir /tmp/mockup \
  --mode mockup \
transfer 0 from bootstrap2 \
              to mockup_testme \
              --arg "(Left (Right 32))" --burn-cap 0.01
```

The network will again send back many information including the updated storage which should now be equal to 42.

This conclude our section about testing. As an exercise, you can write the test for the other entrypoint (decrease and reset).
Once you are sure that the contract work correctly for all the use cases, you can move on to the next section

## Publishing the contract

For deploying the contract on Tezos, we will use the `tezos-client` interface like we did on the previous section.

First, you will need an account address. You can get one for testing at the [faucet](https://teztnets.xyz/ghostnet-faucet).
Download the json file and place it in the `ligo_tutorial` folder. $!$ The account that you get from the faucet are only temporary

Then we are going to point the client on a Tezos node

```zsh
tezos-client --endpoint https://rpc.ghostnet.teztnets.xyz config update
```

This is the testnet, which is a separate network from Tezos, use for testing.

Once done, activate your account

```zsh
tezos-client activate account alice with <the name of the json file>
```

You will receive different messages from the node. The last one should confirm the activation of account Alice

You are now ready to originate your contract

```zsh
tezos-client originate contract increment \
              transferring 0 from alice \
              running increment.tz \
              --init 10 --burn-cap 0.1
```

Again, you will receive several messages from the node and you should get the confirmation that the contract has been published.

You can search your contract on the network using the portal [Better call dev](https://better-call.dev/)

You can know call your contract with

```zsh
tezos-client call increment from alice \
             --arg "(Left (Right 32))" \
             --burn-cap 0.1
```

If you do so, you will see several information on the operation, including the new contract storage.

This conclude this part of our tutorial.
You should now be able to compile, test, publish and call a contract.
Now you can go to the tacos shop tutorial to know more about programming with Ligo or you can start developing your own contract using the Ligo flavor you are more familiar with.
