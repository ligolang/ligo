import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";

export function initialize() {
  let ligoEditor = new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, javascript()],
      doc: `

type storage = int;

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

/* Two entrypoints */

const add = (store: storage, delta: int) => store + delta;
const sub = (store: storage, delta: int) => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */

const main = (action: parameter, store: storage) : [ list<operation> , storage ] => {
 return [
   list([]),    // No operations
   (match (action, {
    Increment: n => add (store, n),
    Decrement: n => sub (store, n),
    Reset:     ()  => 0}))
  ]
};
`,
    }),
    parent: document.getElementById("ligo"),
  });
  let michelsonEditor = new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, javascript()],
      doc: ` `,
    }),
    parent: document.getElementById("michelson"),
  });
  return { ligoEditor, michelsonEditor };
}
