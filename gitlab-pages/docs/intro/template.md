---
title: Starting from a template
---

LIGO provides templates to start projects quickly and to demonstrate specific and complex use cases.
To create a project from a template, use the name of the template and a name for the new project:

```bash
ligo init contract --template [TEMPLATE_NAME] [PROJECT_NAME]
```

For example, to create a project from the [Multisig-Cameligo](https://packages.ligolang.org/contract/Multisig-Cameligo) template, which provides an example of [multi-signature contracts](https://docs.tezos.com/smart-contracts/multisig), run this command:

```bash
ligo init contract --template multisig-cameligo my-multisig-project
```

The templates made by the LIGO team are listed on the [LIGO registry](https://packages.ligolang.org/contracts).

You can also load these templates in the [Online IDE](https://ide.ligolang.org).
