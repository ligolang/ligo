/**
// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  "docs": {
    "Getting started": [
      "intro/introduction",
      {
        "type": "category",
        "label": "Installation",
        "items": [
          "intro/installation",
          "intro/editor-support"
        ]
      },
      "tutorials/getting-started/getting-started"
    ],
    "Writing a Contract": [
      {
        "type": "category",
        "label": "First contract",
        "items": [
          "tutorials/taco-shop/tezos-taco-shop-smart-contract",
          "tutorials/taco-shop/tezos-taco-shop-payout"
        ]
      },
      "tutorials/start-a-project-from-a-template"
    ],
    "Comments": [
      "comments/comments"
    ],
    "Keywords": [
      "keywords/keywords",
      "keywords/escaped_vars"
    ],
    "Constants": [
      "constants/constants",
      "constants/silent_vars"
    ],
    "Numbers": [
      "numbers/declaring",
      "numbers/casting",
      "numbers/adding",
      "numbers/subtracting",
      "numbers/negating",
      "numbers/multiplying",
      "numbers/dividing"
    ],
    "Strings": [
      "strings/strings",
      "strings/concatenating",
      "strings/sizing",
      "strings/slicing",
      "strings/verbatim"
    ],
    "Booleans": [
      "booleans/booleans",
      "booleans/or",
      "booleans/and",
      "booleans/not",
      "booleans/comparing",
      "booleans/conditionals"
    ],
    "Tuples": [
      "tuples/declaring",
      "tuples/accessing"
    ],
    "Functions": [
      "functions/declaring",
      "functions/lambdas",
      "functions/higher-order",
      "functions/inlining",
      "functions/recursion"
    ],
    "Polymorphism": [
      "polymorphism/polymorphism",
      "polymorphism/parametric_types",
      "polymorphism/functions"
    ],
    "Variants": [
      "variants/unit",
      "variants/variants",
      "variants/options",
      "variants/matching"
    ],
    "Side effects": [
      "imperative/mutating",
      "imperative/looping",
      "imperative/failing",
      "imperative/asserting"
    ],
    "Lists": [
      "lists/declaring",
      "lists/adding",
      "lists/matching",
      "lists/updating",
      "lists/folding",
      "lists/mapping",
      "lists/looping"
    ],
    "Records": [
      "records/declaring",
      "records/accessing",
      "records/assigning"
    ],
    "Sets": [
      "sets/declaring",
      "sets/sizing",
      "sets/searching",
      "sets/adding",
      "sets/removing",
      "sets/updating",
      "sets/folding",
      "sets/mapping",
      "sets/iterating",
      "sets/looping"
    ],
    "Maps": [
      "maps/declaring",
      "maps/sizing",
      "maps/searching",
      "maps/adding",
      "maps/removing",
      "maps/updating",
      "maps/folding",
      "maps/mapping",
      "maps/iterating",
      "maps/looping"
    ],
    "Modules/Namespaces": [
      "modules/declaring",
      "modules/accessing",
      "modules/nesting",
      "modules/aliasing",
      "modules/importing",
      "modules/including"
    ],
    "Signatures/Interfaces": [
      "signatures/declaring",
      "signatures/extending"
    ],
    "Switches": [
      "switches/switches"
    ],
    "Preprocessor": [
      "preprocessor/preprocessor",
      "preprocessor/comments",
      "preprocessor/strings",
      "preprocessor/if",
      "preprocessor/define",
      "preprocessor/include",
      "preprocessor/import",
      "preprocessor/error"
    ],
    "Tezos features": [
      "advanced/decorators",
      "advanced/entrypoints-contracts",
      "contract/views",
      "contract/events",
      "language-basics/tezos-specific"
    ],
    "Testing and Debugging": [
      "advanced/testing",
      "advanced/mutation-testing",
      "advanced/michelson_testing"
    ],
    "Combining Code": [
      {
        "type": "doc",
        "id": "language-basics/modules",
        "label": "Modules",
        "customProps": {
          "jsLigoName": "Namespaces"
        }
      },
      "advanced/global-constants",
      "advanced/package-management"
    ],
    "Advanced Topics": [
      "advanced/polymorphism",
      "advanced/inline",
      "advanced/dynamic-entrypoints",
      "tutorials/inter-contract-calls/inter-contract-calls",
      "tutorials/optimisation/optimisation",
      "tutorials/security/security",
      "advanced/timestamps-addresses",
      "advanced/include",
      "advanced/first-contract",
      "advanced/michelson-and-ligo",
      "advanced/interop",
      "advanced/embedded-michelson"
    ],
    "Misc": [
      "intro/editor-support",
      "api/cli-commands",
      "api/cheat-sheet",
      "tutorials/registry/what-is-the-registry",
      "tutorials/registry/how-to-make-an-audit",
      "tutorials/tz-vs-eth/tz-vs-eth"
    ]
  },
  "API": {
    "Language": [
      "reference/map-reference",
      "reference/toplevel-reference",
      "reference/tezos-reference",
      "reference/bitwise-reference",
      "reference/option-reference",
      "reference/string-reference",
      "reference/list-reference",
      "reference/bytes-reference",
      "reference/big-set-reference",
      "reference/set-reference",
      "reference/dynamic-entrypoints-reference",
      "reference/big-map-reference",
      "reference/crypto-reference",
      {
        "type": "category",
        "label": "Test",
        "items": [
          "reference/test-reference",
          "reference/test.proxy-ticket-reference",
          "reference/test.pbt-reference",
          {
            "type": "category",
            "label": "Next",
            "items": [
              "reference/test.next.ticket-reference",
              "reference/test.next.timelock-reference",
              "reference/test.next.compare-reference",
              "reference/test.next.string-reference",
              "reference/test.next.typed-address-reference",
              "reference/test.next.io-reference",
              "reference/test.next-reference",
              "reference/test.next.address-reference",
              "reference/test.next.contract-reference",
              "reference/test.next.originate-reference",
              "reference/test.next.dynamic-entrypoints-reference",
              "reference/test.next.crypto-reference",
              {
                "type": "category",
                "label": "State",
                "items": [
                  "reference/test.next.state-reference",
                  "reference/test.next.state.reset-reference"
                ]
              },
              {
                "type": "category",
                "label": "Account",
                "items": [
                  "reference/test.next.account.contract-reference",
                  "reference/test.next.account-reference"
                ]
              },
              {
                "type": "category",
                "label": "Michelson",
                "items": [
                  "reference/test.next.michelson-reference",
                  "reference/test.next.michelson.contract-reference"
                ]
              },
              {
                "type": "category",
                "label": "Mutation",
                "items": [
                  "reference/test.next.mutation.all-reference",
                  "reference/test.next.mutation-reference"
                ]
              },
              {
                "type": "category",
                "label": "Assert",
                "items": [
                  "reference/test.next.assert.error-reference",
                  "reference/test.next.assert-reference"
                ]
              }
            ]
          }
        ]
      }
    ],
    "CLI": [
      {
        "type": "doc",
        "id": "manpages/ligo"
      },
      {
        "type": "category",
        "label": "ligo compile",
        "items": [
          "manpages/compile constant",
          "manpages/compile contract",
          "manpages/compile expression",
          "manpages/compile parameter",
          "manpages/compile storage"
        ]
      },
      {
        "type": "category",
        "label": "ligo run",
        "items": [
          "manpages/run dry-run",
          "manpages/run evaluate-call",
          "manpages/run evaluate-expr",
          "manpages/run interpret",
          "manpages/run test",
          "manpages/run test-expr"
        ]
      },
      {
        "type": "category",
        "label": "ligo print",
        "items": [
          "manpages/print ast-aggregated",
          "manpages/print ast-core",
          "manpages/print ast-typed",
          "manpages/print ast-expanded",
          "manpages/print ast-unified",
          "manpages/print cst",
          "manpages/print dependency-graph",
          "manpages/print mini-c",
          "manpages/print preprocessed",
          "manpages/print pretty"
        ]
      },
      {
        "type": "category",
        "label": "ligo transpile",
        "items": [
          "manpages/transpile contract",
          "manpages/transpile-with-ast contract",
          "manpages/transpile-with-ast expression"
        ]
      },
      {
        "type": "category",
        "label": "ligo info",
        "items": [
          "manpages/info get-scope",
          "manpages/info list-declarations",
          "manpages/info measure-contract"
        ]
      },
      {
        "type": "category",
        "label": "ligo analytics",
        "items": [
          "manpages/analytics accept",
          "manpages/analytics deny"
        ]
      },
      {
        "type": "category",
        "label": "ligo init",
        "items": [
          "manpages/init contract",
          "manpages/init library"
        ]
      },
      {
        "type": "doc",
        "label": "ligo changelog",
        "id": "manpages/changelog"
      },
      {
        "type": "doc",
        "label": "ligo install",
        "id": "manpages/install"
      },
      {
        "type": "category",
        "label": "ligo registry",
        "items": [
          "manpages/registry add-user",
          "manpages/registry login",
          "manpages/registry publish",
          "manpages/registry unpublish"
        ]
      },
      {
        "type": "doc",
        "label": "ligo repl",
        "id": "manpages/repl"
      }
    ],
    "Changelog": [
      "intro/changelog",
      "protocol/hangzhou",
      "protocol/ithaca",
      "protocol/jakarta",
      "protocol/kathmandu",
      "protocol/lima",
      "protocol/mumbai",
      "protocol/nairobi",
      "protocol/oxford"
    ]
  },
  "faq": {
    "FAQ": [
      "faq/intro",
      "faq/v1-migration-guide",
      "faq/convert-address-to-contract",
      "faq/polymorphic-comparison",
      "faq/catch-error-view",
      "faq/cameligo-ocaml-syntax-diff",
      "faq/tezos-now-advance-time",
      "faq/layout-comb-how",
      "faq/layout-comb-why"
    ]
  }
};

module.exports = sidebars;
