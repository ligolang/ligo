const React = require('react');

const pre = '```';

const PASCALIGO_EXAMPLE = `${pre}pascaligo
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints

function add (const store : storage; const delta : int) : storage is store + delta
function sub (const store : storage; const delta : int) : storage is store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
${pre}`;

const CAMELIGO_EXAMPLE = `${pre}ocaml
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
${pre}`;


const REASONLIGO_EXAMPLE = `${pre}reasonligo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

(* Two entrypoints *)

let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
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
