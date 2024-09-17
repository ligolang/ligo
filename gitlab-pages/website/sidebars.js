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
    "Syntax": [
      "comments/comments",
      "syntax/variables",
      {
        "type": "category",
        "label": "Functions",
        "items": [
          "functions/declaring",
          "functions/lambdas",
          "functions/higher-order",
          "functions/inlining",
          "functions/recursion"
        ]
      },
      {
        "type": "category",
        "label": "Flow control",
        "items": [
          "imperative/looping",
          "imperative/asserting",
          "imperative/switches",
          "imperative/exceptions",
        ],
      },
      {
        "type": "category",
        "label": "Modules",
        "customProps": {
          "jsLigoName": "Namespaces",
        },
        "items": [
          "modules/declaring",
          "modules/accessing",
          "modules/nesting",
          "modules/aliasing",
          "modules/importing",
          "modules/including"
        ]
      },
      {
        "type": "category",
        "label": "Attributes",
        "customProps": {
          "jsLigoName": "Decorators",
        },
        "items": [
          {
            "type": "doc",
            "id": "tezos/decorators/decorators",
            "customProps": {
              "jsLigoName": "Decorators",
            },
          },
          "tezos/decorators/annot",
          "tezos/decorators/deprecated",
          "tezos/decorators/dyn_entry",
          "tezos/decorators/entry",
          "tezos/decorators/inline",
          "tezos/decorators/layout",
          "tezos/decorators/private",
          "tezos/decorators/view"
        ]
      },
      {
        "type": "category",
        "label": "Contracts",
        "items": [
          "tezos/contracts/contracts",
          "tezos/contracts/entrypoints",
          "tezos/contracts/contract-address",
          "tezos/contracts/contract_of",
          "tezos/contracts/michelson",
          "tezos/contracts/michelson-injection",
          "tezos/contracts/operation",
          "contract/events",
          "contract/views",
        ],
      },
      {
        "type": "category",
        "label": "Signatures",
        "customProps": {
          "jsLigoName": "Interfaces",
        },
        "items": [
          "signatures/declaring",
          "signatures/extending"
        ]
      },
      {
        "type": "category",
        "label": "Keywords",
        "items": [
          "keywords/keywords",
          "keywords/escaped_vars"
        ]
      }
    ],
    "Data types": [
      {
        "type": "category",
        "label": "Primitive types",
        "items": [
          "data-types/numbers",
          "data-types/booleans",
          "data-types/strings",
          "data-types/timestamp",
          "data-types/bytes"
        ]
      },
      {
        "type": "category",
        "label": "Complex types",
        "items": [
          "data-types/tuples",
          "data-types/variants",
          "data-types/lists",
          "data-types/records",
          "data-types/sets",
          "data-types/maps"
        ]
      },
      {
        "type": "category",
        "label": "Tezos-specific types",
        "items": [
          "data-types/tez",
          "data-types/key",
          "data-types/hash_key",
          "data-types/signature",
          "data-types/address",
          "data-types/contracts-type",
          "data-types/big_sets",
          "data-types/big_maps",
        ],
      },
      {
        "type": "category",
        "label": "Polymorphism",
        "items": [
          "polymorphism/polymorphism",
          "polymorphism/parametric_types",
          "polymorphism/functions"
        ]
      }
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
    "Testing": [
      "testing/testing",
      "testing/testing-tickets",
      "testing/mutation-testing",
      "advanced/michelson_testing"
    ],
    "Advanced Topics": [
      "advanced/package-management",
      "tutorials/optimisation/optimisation",
      "tutorials/security/security"
    ]
  },
  "API": {
    "Language": [
      "reference/map-reference",
      "reference/bytes-reference",
      "reference/string-reference",
      "reference/option-reference",
      "reference/big-map-reference",
      "reference/list-reference",
      "reference/bitwise-reference",
      "reference/set-reference",
      "reference/toplevel-reference",
      "reference/crypto-reference",
      "reference/big-set-reference",
      "reference/dynamic-entrypoints-reference",
      "reference/tuple2-reference",
      {
        "type": "category",
        "label": "tezos",
        "items": [
          "reference/tezos-reference",
          {
            "type": "category",
            "label": "next",
            "items": [
              "reference/tezos.next.sapling-reference",
              "reference/tezos.next.view-reference",
              "reference/tezos.next.operation-reference",
              "reference/tezos.next.ticket-reference",
              "reference/tezos.next-reference"
            ]
          }
        ]
      },
      {
        "type": "category",
        "label": "test",
        "items": [
          "reference/test.pbt-reference",
          "reference/test.proxy-ticket-reference",
          "reference/test-reference",
          {
            "type": "category",
            "label": "next",
            "items": [
              "reference/test.next.originate-reference",
              "reference/test.next.typed-address-reference",
              "reference/test.next.timelock-reference",
              "reference/test.next.compare-reference",
              "reference/test.next.address-reference",
              "reference/test.next.dynamic-entrypoints-reference",
              "reference/test.next.io-reference",
              "reference/test.next.ticket-reference",
              "reference/test.next-reference",
              "reference/test.next.contract-reference",
              "reference/test.next.string-reference",
              "reference/test.next.crypto-reference",
              {
                "type": "category",
                "label": "state",
                "items": [
                  "reference/test.next.state-reference",
                  "reference/test.next.state.reset-reference"
                ]
              },
              {
                "type": "category",
                "label": "michelson",
                "items": [
                  "reference/test.next.michelson.contract-reference",
                  "reference/test.next.michelson-reference"
                ]
              },
              {
                "type": "category",
                "label": "mutation",
                "items": [
                  "reference/test.next.mutation.all-reference",
                  "reference/test.next.mutation-reference"
                ]
              },
              {
                "type": "category",
                "label": "account",
                "items": [
                  "reference/test.next.account.contract-reference",
                  "reference/test.next.account-reference"
                ]
              },
              {
                "type": "category",
                "label": "assert",
                "items": [
                  "reference/test.next.assert.error-reference",
                  "reference/test.next.assert-reference"
                ]
              }
            ]
          }
        ]
      },
      {
        "type": "category",
        "label": "assert",
        "items": [
          "reference/assert-reference",
          "reference/assert.error-reference"
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
      "faq/convert-address-to-contract",
      "faq/polymorphic-comparison",
      "faq/catch-error-view",
      "faq/cameligo-ocaml-syntax-diff",
      "faq/tezos-now-advance-time"
    ]
  }
};

module.exports = sidebars;
