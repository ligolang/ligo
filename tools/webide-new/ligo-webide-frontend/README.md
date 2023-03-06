# Ligo Web IDE frontend

## Build

On `ligo-ide` run `yarn install` to install dependencies, `yarn build:react` to build project and `yarn dev:react` will start it on `localhost:3000`. \
Also for working with `git` you need to specify `GIT_PROXY` env variable. This proxy is required by one of the dependencies, see https://github.com/isomorphic-git/isomorphic-git#cors-support. For connecting to the backend, you need to specify a `BACKEND_URL` env variable.

To build with Docker, provide backend url with the arg `--build-arg backend_url`.
To generate a release, push a new branch named `webide-release/frontend` on https://gitlab.com/ligolang/ligo. The CI will generate a new image and push it into [gitlab container registry](https://gitlab.com/ligolang/ligo/container_registry/3546999)

## Deploy

For deployment you need to run `yarn build:react-prod`. It include `NODE_ENV=production` and `PUBLIC_URL=/` env variable. Also you need to specify same `GIT_PROXY` and `BACKEND_URL` env variables as on the previous point.

### Network and protocol change

To specify networks and protocols we provide `config.json` file in `ligo-ide`. \
Here you have such options:
- `protocols`: the list of protocols which are allowed. Here you need to specify `name` which is used in ligo compiler and `showName`.
- `defaultProtocol`: specific element from `protocols`
- `networks`: list of allowed networks. You need to specify unique `id`, `group` for network groups such as `Tezos` or `T4L3NT`, `name` where `Mainnet` is used for main chain and other names will be joined together as testnets, `fullName` as display name, `url` of the node for specific chain, `explorerUrl`, `symbol` like `XTZ` of the chain coin, and `icon` with `type`. 

__Note__: `icon` is a map to asset logo in the project. To add it, put logo to `./ligo-ide/src/ligo-components/eth-sdk/assets`, import it in `./ligo-ide/src/ligo-components/eth-sdk/NetworkIcon.ts` and add to the `./ligo-ide/src/ligo-components/eth-sdk/networks.js`.

__Note__: `type` is a `NetworkType` for `beacon` wallet. We use `@taquito/beacon-wallet` for it, so you should check what version of `@airgap/beacon-dapp` is currently available and what networks available in this version. You can get network updates info in `@airgap/beacon-dapp` [changelog](https://github.com/airgap-it/beacon-sdk/releases) and `@airgap/beacon-dapp` updates in `taquito` [changelog](https://tezostaquito.io/docs/version/). And in case of any updates upgrade dependencies. If network is not available in current version you should use `custom` type.

## Licensing

This project is a fork of the Black IDE by Obsidian Labs, available at
https://github.com/ObsidianLabs/Black-IDE. The project was forked on April 13,
2022. Therefore, changes prior to April 13, 2022 are copyrighted by the Obisidian
Labs contributors, licensed under GPL v3. Any changes after April 13, 2022 are
copyrighted by LIGO. 
