# Ligo Web IDE frontend

## Prerequistes

Ensure that you are using a Node.js 16, since Node.js >= 17 doesn't work yet
because the application uses legacy OpenSSL configuration options. See
[here](https://exerror.com/opensslerrorstack-error03000086digital-envelope-routinesinitialization-error/)
for more information.

## Build

On `ligo-ide` run `yarn install` to install dependencies, `yarn build:react` to build project and `yarn dev:react` will start it on `localhost:3000`.

## Licensing

This project is a fork of the Black IDE by Obsidian Labs, available at
https://github.com/ObsidianLabs/Black-IDE. The project was forked on April 13,
2022. Therefore, changes prior to April 13, 2022 are copyrighted by the Obisidian
Labs contributors, licensed under GPL v3. Any changes after April 13, 2022 are
copyrighted by LIGO. 
