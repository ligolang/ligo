---
id: ligo-snippets-demo
title: Ligo-Snippets Demo
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

“ligo-snippets” (https://www.npmjs.com/package/@ligolang/ligo-snippets) is a React component that can be included on any webpage to display Ligo source code to users.

The user will see Ligo code with syntax highlighting, and an action button allowing the user to open the source code in the Ligo Web IDE (https://ide.ligolang.org).

Each code snippet can have preset Ligo Web IDE configurations (e.g. entrypoint, parameters or storage). These configurations are optional and will be passed onto the Ligo Web IDE when present. This will allow examples to provide the proper configurations for the reader to experiment with.

The “ligo-snippets” React component uses the CodeJar editor
(https://github.com/antonmedv/codejar), which is extremely lightweight
(only 2kB).  It currently supports syntax highlighting for
CameLigo. Additionally, it has both a light and dark theme mode.

<Tabs
  defaultValue="cameligo"
  values={[
    { label: 'CameLIGO', value: 'cameligo', },
  ]
}>
<TabItem value="cameligo">

```cameligo {"name": "Ligo Introduction Example", "editor": true}
(*_*
  name: CameLIGO Contract
  language: cameligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment 1
    storage: 999
  deploy:
    entrypoint: main
    storage: 999
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: 5, 6
  generateDeployScript:
    entrypoint: main
    storage: 999
*_*)
type storage = int
type result = operation list * storage

[@entry] let add (a : int) (store : storage) : result = [], a + b
[@entry] let sub (a : int) (store : storage) : result = [], a - b
```

</TabItem>

</Tabs>

<!-- updated use of entry -->