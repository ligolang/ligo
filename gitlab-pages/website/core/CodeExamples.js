const React = require('react');

const pre = '```';

const PASCALIGO_EXAMPLE = `${pre}pascaligo
// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is a + b

function subtract (const a : int ; const b : int) : int is a - b

// real entrypoint that re-routes the flow based on the action provided
function main (const p : action ; const s : int) : (list(operation) * int) is 
  ((nil : list(operation)),
    case p of
    | Increment (n) -> add (s, n)
    | Decrement (n) -> subtract (s, n)
    end)
${pre}`;

const CAMELIGO_EXAMPLE = `${pre}ocaml
type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type action =
| Increment of int
| Decrement of int

let add (a,b: int * int) : int = a + b
let sub (a,b: int * int) : int = a - b

(* real entrypoint that re-routes the flow based on the action provided *)

let main (p,s: action * storage) =
 let storage =
   match p with
   | Increment n -> add (s, n)
   | Decrement n -> sub (s, n)
 in ([] : operation list), storage
${pre}`;


const REASONLIGO_EXAMPLE = `${pre}reasonligo
type storage = int;

/* variant defining pseudo multi-entrypoint actions */

type action =
  | Increment(int)
  | Decrement(int);

let add = ((a,b): (int, int)): int => a + b;
let sub = ((a,b): (int, int)): int => a - b;

/* real entrypoint that re-routes the flow based on the action provided */

let main = ((p,storage): (action, storage)) => {
  let storage =
    switch (p) {
    | Increment(n) => add((storage, n))
    | Decrement(n) => sub((storage, n))
    };
  ([]: list(operation), storage);
};
${pre}`;


module.exports = props => {
  const MarkdownBlock = props.MarkdownBlock;

  return (
    <div className="tabs">
      <div className="nav-tabs">
        <div
          className="nav-link active"
          data-group="examples"
          data-tab="pascaligo"
        >
          PascaLIGO
        </div>
        <div className="nav-link" data-group="examples" data-tab="cameligo">
          CameLIGO
        </div>
        <div className="nav-link" data-group="examples" data-tab="reasonligo">
          ReasonLIGO
        </div>
      </div>
      <div className="tab-content">
        <div id="pascaligo" className="tab-pane active" data-group="examples">
          <MarkdownBlock>{PASCALIGO_EXAMPLE}</MarkdownBlock>
        </div>
        <div id="cameligo" className="tab-pane" data-group="examples">
          <MarkdownBlock>{CAMELIGO_EXAMPLE}</MarkdownBlock>
        </div>
        <div id="reasonligo" className="tab-pane" data-group="examples">
          <MarkdownBlock>{REASONLIGO_EXAMPLE}</MarkdownBlock>
        </div>
      </div>
    </div>
  );
};
