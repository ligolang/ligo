/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  docs: {
    Intro: ["intro/introduction", "intro/installation", "intro/editor-support"],
    "Language Basics": [
      "language-basics/types",
      "language-basics/constants-and-variables",
      "language-basics/math-numbers-tez",
      "language-basics/strings-bytes",
      "language-basics/functions",
      "language-basics/boolean-if-else",
      "language-basics/loops",
      "language-basics/unit-option-pattern-matching",
      "language-basics/maps-records",
      "language-basics/sets-lists-tuples",
      "language-basics/exceptions",
      "language-basics/modules",
      "language-basics/tezos-specific",
    ],
    Advanced: [
      "advanced/timestamps-addresses",
      "advanced/entrypoints-contracts",
      "advanced/include",
      "advanced/first-contract",
      "advanced/michelson-and-ligo",
      "advanced/polymorphism",
      "advanced/testing",
      "advanced/mutation-testing",
      "advanced/michelson_testing",
      "advanced/inline",
      "advanced/interop",
      "advanced/embedded-michelson",
      "advanced/package-management",
      "advanced/global-constants",
    ],
    Reference: ["api/cli-commands", "api/cheat-sheet"],
    "Sub-Commands": [
      "manpages/changelog",
      "manpages/add-user",
      "manpages/compile constant",
      "manpages/compile contract",
      "manpages/compile expression",
      "manpages/compile parameter",
      "manpages/compile storage",
      "manpages/info get-scope",
      "manpages/info list-declarations",
      "manpages/info measure-contract",
      "manpages/init contract",
      "manpages/init library",
      "manpages/install",
      "manpages/ligo",
      "manpages/login",
      "manpages/mutate ast",
      "manpages/mutate cst",
      "manpages/print ast-aggregated",
      "manpages/print ast-core",
      "manpages/print ast-imperative",
      "manpages/print ast-typed",
      "manpages/print cst",
      "manpages/print dependency-graph",
      "manpages/print mini-c",
      "manpages/print preprocessed",
      "manpages/print pretty",
      "manpages/publish",
      "manpages/repl",
      "manpages/run dry-run",
      "manpages/run evaluate-call",
      "manpages/run evaluate-expr",
      "manpages/run interpret",
      "manpages/run test",
      "manpages/run test-expr",
      "manpages/transpile contract",
      "manpages/transpile expression",
    ],
    API: [
      "reference/toplevel",
      "reference/big-map-reference",
      "reference/bitwise-reference",
      "reference/bytes-reference",
      "reference/crypto-reference",
      "reference/list-reference",
      "reference/map-reference",
      "reference/set-reference",
      "reference/string-reference",
      "reference/option-reference",
      "reference/current-reference",
      "reference/test",
    ],
    "Protocol Upgrades": [
      "protocol/hangzhou",
      "protocol/ithaca",
      "protocol/jakarta",
      "protocol/kathmandu",
      "protocol/lima",
    ],
  },
  "contributors-docs": {
    Introduction: [
      "contributors/origin",
      "contributors/philosophy",
      "contributors/getting-started",
      "contributors/documentation-and-releases",
    ],
    "Big Picture": [
      "contributors/big-picture/overview",
      "contributors/big-picture/front-end",
      "contributors/big-picture/middle-end",
      "contributors/big-picture/back-end",
      "contributors/big-picture/vendors",
    ],
    "Road Map": [
      "contributors/road-map/short-term",
      "contributors/road-map/long-term",
    ],
  },
  tutorials: [
    {
      label: "Getting started",
      type: "doc",
      id: "tutorials/getting-started/getting-started",
    },
    {
      type: "category",
      label: "First contract",
      collapsible: true,
      collapsed: true,
      items: [
        "tutorials/taco-shop/tezos-taco-shop-smart-contract",
        "tutorials/taco-shop/tezos-taco-shop-payout",
      ],
    },
    {
      type: "category",
      label: "LIGO Registry",
      collapsible: true,
      collapsed: true,
      items: [
        "tutorials/registry/what-is-the-registry",
        "tutorials/registry/how-to-make-an-audit",
      ],
    },
    {
      type: "doc",
      id: "tutorials/start-a-project-from-a-template",
    },
    {
      type: "doc",
      label: "Migrating from Ethereum",
      id: "tutorials/tz-vs-eth/tz-vs-eth",
    },
    {
      type: "doc",
      label: "Inter-contract invocations",
      id: "tutorials/inter-contract-calls/inter-contract-calls",
    },
    {
      type: "doc",
      label: "Smart contract security",
      id: "tutorials/security/security",
    },
    {
      label: "Optimisation",
      type: "doc",
      id: "tutorials/optimisation/optimisation",
    },
  ],
  faq: {
    FAQ: [
      "faq/intro",
      "faq/convert-address-to-contract",
      "faq/polymorphic-comparison",
      "faq/catch-error-view",
      "faq/cameligo-ocaml-syntax-diff",
      "faq/tezos-now-advance-time",
    ],
  },
};

module.exports = sidebars;
