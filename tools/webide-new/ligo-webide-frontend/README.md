# Ligo Web IDE frontend

## Build

1. Run `yarn install` on each of three projects.
2. Run `yarn build` on `base-components` and `ligo-components`.
3. On `ligo-ide` run `yarn build:react` to build project and `yarn dev:react` will start it on `localhost:3000`.
4. To make changes when your app is launched you just need to save your changes. Some packages in `base-components` and `ligo-components` may require rebuild. You can do it using `yarn build` in specific package forder.
