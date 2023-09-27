---
id: introduction
title: Introduction to LIGO
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

### Ligo, a SmartContract Language built for Tezos

LIGO is a programming language for writing [smart contracts](https://opentezos.com/tezos-basics/smart-contracts), compiling in [Michelson](https://opentezos.com/michelson) and deployable on [Tezos blockchain](https://tezos.com/).

Our hope is to have a simple, strongly typed language with
a low footprint. Most useful smart contracts can express their core functionality in under a
thousand lines of code.

### Ligo, for newcomers or confirmed developpers

Even if LIGO currently offers **two syntaxes**, you'll need to **choose only one** :

  - **JsLIGO**, ideal for web developers, is a TypeScript/JavaScript inspired syntax without unnecessary complexity, which is not helpful in smart contract development. A quick way to produce your first dApp!


```jsligo
  type storage = string;

  @entry
  const store_hello = (delta: int, store: storage): [list<operation>, storage] =>
    [list([]), "Hello"];
```

  - **CameLIGO**,  is designed for functional developers like tezos core devs. An [OCaml-inspired](https://ocaml.org/)  syntax allows you to write in a functional style.


```cameligo
  type storage = string

  [@entry]
  let store_hello (delta : int) (store : storage) : operation list * storage = [], "Hello"
```

A significant advantage of the multi-syntax feature is to share knowledge, toolings, and [modules](https://ligolang.org/docs/language-basics/modules) (like [libraries](https://ligolang.org/docs/advanced/package-management) onto [registry](https://packages.ligolang.org/packages)) in a larger community.

### Ligo, designed to be cost-effective

Unlike desktop, mobile, or web application development, smart contracts cannot rely on cheap CPU time and memory.
All resources contracts use are expensive and tracked as ['gas costs'](https://ligolang.org/docs/tutorials/optimisation/#tezos-gas-model).

Ligo compiler will generate optimized Michelson code, which will be cost-effective on tezos.


### Ligo, designed for your security

Tezos smart contract  live on the blockchain forever  if a bug exists, they can't be patched or amended.
Smart contracts often directly control money or assets, which if stolen, could be a large financial loss to the contracts and their users.

Ligo will **bring people to web3** and by design **reduce the risk** that your smart contract will lose its balance to an
 [avoidable exploit](https://www.wired.com/2016/06/50-million-hack-just-showed-dao-human/).  

But compiler design is insufficient, ligo use **static analysis** to encourage people to write simple code, avoid anti-patterns, and use the [robust test framework](https://ligolang.org/docs/advanced/testing) which can simulate tezos blockchain and offer [mutation tests](https://ligolang.org/docs/advanced/mutation-testing)

For critical code, LIGO also keeps its compiled output unbloated making **possible to formally verify** the compiled output using a project like
[Mi-Cho-Coq](https://gitlab.com/nomadic-labs/mi-cho-coq/).

### A set of tools already available

- Quickly explore Ligo using [webide](https://ide.ligolang.org/local)
- Quickly bootstrap a project with [registry](https://packages.ligolang.org/packages)
- Improve development experience with our lsp server available in [vscode extension](https://marketplace.visualstudio.com/items?itemName=ligolang-publish.ligo-vscode)
- Understand and troubleshoot your code with the debugger available in [vscode extension](https://marketplace.visualstudio.com/items?itemName=ligolang-publish.ligo-vscode)
- Integrate Ligo to your pipeline with [ligo github action](https://github.com/marigold-dev/ligo-action)
- Test your documentation with [ligo-mdx](https://github.com/ligolang/ligo-mdx)

---

# Where to start

### Do you want to try Ligo ?
For a quick overview, [get-started]( https://ligolang.org/docs/tutorials/getting-started) is a good choice. [Webide](https://ide.ligolang.org/) can be used to avoid installation onto your laptop.

### Do you want to learn Ligo ?
Your choice to learn Ligo is already available :
- Read [basics](https://ligolang.org/docs/language-basics/types) to have a basic comprehension
- Write your first [smart contract](https://ligolang.org/docs/tutorials/taco-shop/tezos-taco-shop-smart-contract).
- Others ressources are available on [marigold.dev](https://www.marigold.dev/learn)

### Do you want to build a production-ready project ?
You will need a deeper comprehension : 
- Teach yourself how to structure your code with [Combining code](https://ligolang.org/docs/next/language-basics/modules) section
- Learn how to [write tests](https://ligolang.org/docs/next/advanced/testing?lang=jsligo) we strongly encourage to use [breathalyzer library from the LIGO registry.](https://packages.ligolang.org/package/ligo-breathalyzer)
- Understand how to [secure a contract](https://ligolang.org/docs/tutorials/security)

### Go deeper
In the end, maybe you will want to :
- [Optimize your code](https://ligolang.org/docs/tutorials/optimisation/), 
- Understand [link between Ligo and Michelson](https://ligolang.org/docs/advanced/michelson-and-ligo)
- [Interact with other contracts](https://ligolang.org/docs/tutorials/inter-contract-calls/)

Consult [Advanced topics section](https://ligolang.org/docs/advanced/polymorphism).

<!-- updated use of entry -->