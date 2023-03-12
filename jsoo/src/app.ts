import { EditorState } from "@codemirror/state";
import { basicSetup } from "codemirror";
// @ts-ignore
import _BLS12381 from "@ligolang/ocaml-bls12-381";
// @ts-ignore
import HaclWasm from "@ligolang/hacl-wasm";
// @ts-ignore
import _SECP256K1 from "@ligolang/secp256k1-wasm";
import * as Editor from "./editor";

async function initialize() {
  // @ts-ignore
  window._BLS12381 = await _BLS12381();
  // @ts-ignore
  window._HACL = await HaclWasm.getInitializedHaclModule();
  // @ts-ignore
  window._SECP256K1 = await _SECP256K1();
}

async function loadJSBundle(path: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = path;
    script.onload = function () {
      resolve();
    };
    script.onerror = function (e) {
      reject(e);
    };
    document.head.appendChild(script);
  });
}

async function main() {
  // @ts-ignore
  // window.ligo.compile();
  let { ligoEditor, michelsonEditor } = Editor.initialize();
  await initialize();
  let app = "js_main";
  let path = "";
  // No runtime file needed since we just enable effects,
  // and it can only be enabled in whole program compilation
  // mode with Dune
  // await loadJSBundle(`${path}/${app}.bc.runtime.js`);
  await loadJSBundle(`${path}/${app}.bc.js`);
  console.log("All WASM dependencies loaded");
  document.getElementById("compile")?.addEventListener("click", function () {
    // @ts-ignore
    let michelson = window.ligo.compile(
      ligoEditor.state.doc.toJSON().join("\n"),
      // @ts-ignore
      document.getElementById("syntax")?.value
    );
    console.log(michelson);
    michelsonEditor.setState(
      EditorState.create({
        extensions: [basicSetup],
        doc: michelson,
      })
    );
  });
  document.getElementById("test")?.addEventListener("click", function () {
    // @ts-ignore
    let michelson = window.ligo.test(
      ligoEditor.state.doc.toJSON().join("\n"),
      // @ts-ignore
      document.getElementById("syntax")?.value
    );
    console.log(michelson);
    michelsonEditor.setState(
      EditorState.create({
        extensions: [basicSetup],
        doc: michelson,
      })
    );
  });
}

main();
