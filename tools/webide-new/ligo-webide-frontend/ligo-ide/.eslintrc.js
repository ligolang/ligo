// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: CC0-1.0

module.exports = {
  extends: [
    "airbnb",
    "airbnb-typescript",
    "airbnb/hooks",
    "plugin:@typescript-eslint/recommended",
    "plugin:@typescript-eslint/recommended-requiring-type-checking",
    "plugin:prettier/recommended",
    "prettier",
  ],

  parserOptions: {
    project: ["./tsconfig.json"],
  },

  // "import/extensions": [".js", ".jsx", ".ts", ".tsx"],

  ignorePatterns: ["/*.*"],

  plugins: ["@typescript-eslint"],
  parser: "@typescript-eslint/parser",
  rules: {
    // Sometime we need to specify any type, so it's rather useful
    "@typescript-eslint/no-explicit-any": 0,

    "react/jsx-closing-bracket-location": 0,

    // Import modules from different places
    "import/no-unresolved": 0,

    // JSX extensions support
    "react/jsx-filename-extension": [
      2,
      {
        extensions: [".jsx", ".tsx", ".js", ".ts"],
      },
    ],

    quotes: "off",
    "@typescript-eslint/quotes": [2, "double", { avoidEscape: true }],
    'no-plusplus': [2, { allowForLoopAfterthoughts: true }],

    "@typescript-eslint/comma-dangle":
      ['error', {
        arrays: 'only-multiline',
        objects: 'only-multiline',
        imports: 'only-multiline',
        exports: 'only-multiline',
        functions: 'never', },],

    // Use TS no-unused-vars rule
    "no-unused-vars": "off",
    "@typescript-eslint/no-unused-vars": ["error"],
    "@typescript-eslint/no-use-before-define": 0,

    // Support declarations in case
    "no-case-declarations": 0,

    // We already write props type in generic of functional component
    "react/prop-types": 0,

    "no-alert": 0, // TODO enable it later

    // Rules for making interface available for people with disabilities
    "jsx-a11y/control-has-associated-label": 0,
    "jsx-a11y/click-events-have-key-events": 0,
    "jsx-a11y/no-static-element-interactions": 0,

    // Enable while(true) loops
    "no-constant-condition": ["error", { checkLoops: false }],

    "react/jsx-props-no-spreading": 0,
    "jsx-a11y/tabindex-no-positive": 0,
    "react/require-default-props": 0,
    "no-nested-ternary": 0,
    "import/prefer-default-export": 0,

    "import/extensions": 0,

    'no-restricted-syntax': [ 
      'error', 
      { 
        selector: 'ForInStatement', 
        message: 'for..in loops iterate over the entire prototype chain, which is virtually never what you want. Use Object.{keys,values,entries}, and iterate over the resulting array.', 
      },
      { 
        selector: 'LabeledStatement', 
        message: 'Labels are a form of GOTO; using them makes code confusing and hard to maintain and understand.', 
      }, 
      { 
        selector: 'WithStatement', 
        message: '`with` is disallowed in strict mode because it makes code impossible to predict and optimize.', 
      }, 
    ],
  },
  overrides: [
    {
      files: ["*.js", "*.jsx"],
      rules: {
        "@typescript-eslint/no-unsafe-return": 0,
        "@typescript-eslint/no-unsafe-member-access": 0,
        "@typescript-eslint/no-unsafe-assignment": 0,
        "@typescript-eslint/explicit-module-boundary-types": 0,
        "@typescript-eslint/no-unsafe-call": 0,
        "@typescript-eslint/restrict-template-expressions": 0,
        "@typescript-eslint/return-await": 0,
        "@typescript-eslint/no-shadow": 0,
        "@typescript-eslint/no-floating-promises": 0,
        "@typescript-eslint/require-await": 0,
        "@typescript-eslint/unbound-method": 0,
        "@typescript-eslint/no-unsafe-argument": 0,
        "@typescript-eslint/await-thenable": 0,
        "@typescript-eslint/no-useless-constructor": 0,
        "@typescript-eslint/no-unused-expressions": 0,
        "@typescript-eslint/no-unused-vars": 0,
        "@typescript-eslint/no-var-requires": 0,
        "@typescript-eslint/restrict-plus-operands": 0,
        "@typescript-eslint/no-misused-promises": 0,
        "@typescript-eslint/naming-convention": 0,
        "@typescript-eslint/no-loop-func": 0,
        "@typescript-eslint/no-empty-function": 0,
        "@typescript-eslint/prefer-regexp-exec": 0,

        // TODO: remove or update
        "import/extensions": 0,
        "class-methods-use-this": 0,
        "max-classes-per-file": 0,
        "no-underscore-dangle": 0,
        "consistent-return": 0,
        "radix": 0,
        "no-restricted-exports": 0,
        "react/destructuring-assignment": 0,
        "no-param-reassign": 0,
        "react/no-access-state-in-setstate": 0,
        "react/no-unused-state": 0,
        "react/no-unused-class-component-methods": 0,
        "react/static-property-placement": 0,
        "react/forbid-prop-types": 0,
        "default-case": 0,
        "react/state-in-constructor": 0,
        "react/sort-comp": 0,
        "no-empty": 0,
        "jsx-a11y/anchor-is-valid": 0,
        "react/no-array-index-key": 0,
        "no-return-assign": 0,
        "func-names": 0,
        "new-cap": 0,
        "no-promise-executor-return": 0,
        "no-restricted-syntax": 0,
        "guard-for-in": 0,
        "no-continue": 0,
        "react/prefer-stateless-function": 0,
        "react/jsx-no-bind": 0,
        "react/no-unstable-nested-components": 0,
        "react/no-children-prop": 0,
        "prefer-const": 0,
        "no-undef": 0,
        "react-hooks/exhaustive-deps": 0,
        "react/no-unused-prop-types": 0,
        "jsx-a11y/alt-text": 0,
        "jsx-a11y/no-noninteractive-element-interactions": 0,
        "global-require": 0,
        "no-void": 0,
        "react/jsx-no-undef": 0,
        "prefer-destructuring": 0,
        "react/jsx-no-script-url": 0,
        "no-script-url": 0,
        "react/jsx-no-duplicate-props": 0,
        "no-fallthrough": 0,
        "import/no-named-as-default-member": 0,
        "import/no-named-as-default": 0,
        "import/no-named-default": 0,
        "no-unsafe-optional-chaining": 0,
        "react-hooks/rules-of-hooks": 0,
        "import/no-cycle": 0,
        "react/no-find-dom-node": 0,
        "react/button-has-type": 0,
        "no-await-in-loop": 0,
        "import/no-dynamic-require": 0,
        "no-restricted-globals": 0
      },
    },
  ],
  "env": {
    "es2020": true
  },
  globals: {
    window: true,
    alert: true,
    document: true,
    Node: true,
  },
};
