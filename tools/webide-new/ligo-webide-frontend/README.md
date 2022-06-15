# Ligo Web IDE frontend

## Build

1. Run `yarn install` on each of three projects.
2. Run `yarn build` on `base-components` and `ligo-components`.
3. On `ligo-ide` run `yarn build:react` to build project and `yarn dev:react` will start it on `localhost:3000`.
4. To make changes when your app is launched you just need to save your changes. Some packages in `base-components` and `ligo-components` may require rebuild. You can do it using `yarn build` in specific package forder.

## Licensing

This project is a fork of the Black IDE by Obsidian Labs, available at
https://github.com/ObsidianLabs/Black-IDE. The project was forked on April 13,
2022. Therefore, changes prior to April 13, 2022 are copyrighted by the Obisidian
Labs contributors, licensed under GPL v3. Any changes after April 13, 2022 are
copyrighted by Serokell OÜ.
