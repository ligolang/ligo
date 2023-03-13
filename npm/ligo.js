import _BLS12381 from "@ligolang/ocaml-bls12-381";
import HaclWasm from "@ligolang/hacl-wasm";
import _SECP256K1 from "@ligolang/secp256k1-wasm";

async function initialize() {
  window._BLS12381 = await _BLS12381();
  window._HACL = await HaclWasm.getInitializedHaclModule();
  window._SECP256K1 = await _SECP256K1();
}

async function loadJSBundle(path) {
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

export async function compile(code, syntax) {
  await initialize();
  // No runtime file needed since we just enable effects,
  // and it can only be enabled in whole program compilation
  // mode with Dune
  // await loadJSBundle(`${path}/${app}.bc.runtime.js`);
  await loadJSBundle(new URL(`./js_main.bc.js`, import.meta.url));
  // Make sure the path to js_main.bc.js is written by hand and not computed at runtime with variables
  // Otherwise, webpack and rollup wont replace the import.meta.url
  // The following wont work
  // await loadJSBundle(new URL(`${path}/${app}.bc.js`, import.meta.url));
  return window.ligo.compile(code, syntax);
}

// Note: rollup expects an es6 module at the consumer end. If necessary, also distribute an es6 export
