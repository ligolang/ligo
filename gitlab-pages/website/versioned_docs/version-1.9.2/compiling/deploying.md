---
id: deploying
title: Deploying contracts
---

LIGO doesn't include a built-in way to deploy (originate) contracts to Tezos.
You can deploy contracts in the [LIGO IDE](https://ide.ligolang.org/) or use other tools that can deploy Tezos contracts.

One popular tool is the
[Octez client](https://octez.tezos.com/docs/user/setup-client.html), which is a
command-line client that runs many different kinds of Tezos operations. For
installation instructions, see
[Installing the Octez client](https://docs.tezos.com/developing/octez-client/installing)
on docs.tezos.com.

Here are general steps for deploying a LIGO contract with the Octez client:

1. Compile the contract as described in [Compiling contracts](../compiling).

1. Get the initial storage value for the contract in Michelson format.
You can use LIGO to compile it as described in [Compiling storage](../compiling#compiling-storage).

1. Install the Octez client and configure it for the target Tezos network.
For example, to configure the client for the Ghostnet test network, get the URL of a Ghostnet node from https://teztnets.com and use it in the `octez-client config update` command, as in this example:

   ```bash
   octez-client --endpoint https://rpc.ghostnet.teztnets.com config update
   ```

   If you are using a testnet, Octez shows a warning that you are not using Mainnet.
   You can hide this message by setting the `TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER` environment variable to "YES".

1. In the Octez client, create or import a wallet as described in
   [Creating accounts](https://docs.tezos.com/developing/octez-client/accounts) on docs.tezos.com.

1. Fund the wallet with tez tokens.
If you are using a testnet, you can use the network's faucet to get free tez.
Testnet faucets are listed on https://teztnets.com.

1. Use the `octez-client originate contract` command to deploy the contract with its initial storage, as in this example:

   ```bash
   octez-client originate contract my_contract transferring 0 from my_account \
     running my_contract.tz --init 0 --burn-cap 1
   ```

   The response includes the address of the deployed contract, which starts with `KT1`.

1. Verify that the contract deployed successfully by finding it on a block explorer:

   1. Open a Tezos block explorer such as [TzKT](https://tzkt.io) or [Better Call Dev](https://better-call.dev/).

   1. Set the explorer to the network that you deployed the contract to.

   1. Paste the contract address into the search field and press Enter.

   1. Go to the Storage tab to see that the initial value of the storage is the value that you provided.

Now you can call the contract from a block explorer, a dApp, or the Octez client.

For a full walkthrough, see [Quickstart](../tutorials/getting-started/).
