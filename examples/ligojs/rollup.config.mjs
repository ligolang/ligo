import commonjs from "@rollup/plugin-commonjs";
import resolve from "@rollup/plugin-node-resolve";
import nodePolyfills from "rollup-plugin-polyfill-node";
import json from "@rollup/plugin-json";
import { importMetaAssets } from "@web/rollup-plugin-import-meta-assets";

export default {
  input: "./app.js",
  output: {
    dir: "./dist",
    format: "iife",
  },
  plugins: [
    importMetaAssets({
      include: [
        "node_modules/ligolang/*.js",
        "node_modules/@ligolang/secp256k1-wasm/src/*.js",
        "node_modules/@ligolang/hacl-wasm/*.js",
        "node_modules/@ligolang/ocaml-bls12-381/dist/*.js",
        "./*.js",
      ],
    }),
    json(),
    commonjs(),
    resolve(),
    nodePolyfills(/* options */),
  ],
};
