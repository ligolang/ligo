const incrementM = `module Counter = struct
  type storage = int

  type ret = operation list * storage

  (* Three entrypoints *)

  [@entry]
  let increment (delta : int) (store : storage) : ret = [], store + delta

  [@entry]
  let decrement (delta : int) (store : storage) : ret = [], store - delta

  [@entry]
  let reset (() : unit) (_ : storage) : ret = [], 0
end
`;

const incrementJ = `export namespace Counter {
  export type storage = int;
  type ret = [list<operation>, storage];

  // Three entrypoints

  @entry
  const increment = (delta : int, store : storage) : ret =>
    [list([]), store + delta];

  @entry
  const decrement = (delta : int, store : storage) : ret =>
    [list([]), store - delta];

  @entry
  const reset = (_u : unit, _s : storage) : ret =>
    [list([]), 0];
};
`;

const testM = `#import "../contracts/Contract.mligo" "Contract"

(* Tests example *)

let initial_storage = 42

let test_initial_storage =
  let {addr; code=_; size=_} =
    Test.originate
      (contract_of Contract.Counter)
      initial_storage
      0mutez in
  assert (Test.get_storage addr = initial_storage)

let test_increment =
  let {addr; code=_; size=_} =
    Test.originate
      (contract_of Contract.Counter)
      initial_storage
      0mutez in
  let contr = Test.to_contract addr in
  let _ = Test.transfer_to_contract_exn contr (Increment 1) 1mutez in
  assert (Test.get_storage addr = initial_storage + 1)
`;

const testJ = `#import "../contracts/Contract.jsligo" "Contract"

const test_initial_storage =
  (
    () => {
      let initial_storage = 42;
      let {addr, code:_c, size:_s} =
        Test.originate(contract_of (Contract.Counter), initial_storage, 0 as tez);
      return assert(Test.get_storage(addr) == initial_storage)
    }
  )();

const test_increment =
  (
    () => {
      let initial_storage = 42;
      let {addr, code:_c, size:_s} =
        Test.originate(contract_of (Contract.Counter), initial_storage, 0 as tez);
      let contr = Test.to_contract(addr);
      let _ = Test.transfer_to_contract_exn(contr, (Increment(1)), 1 as mutez);
      return assert(Test.get_storage(addr) == initial_storage + 1)
    }
  )();
`;

const incrementMStorage = "0";

const incrementJStorage = "0";

const config = (name: string, projectName: string, syntax: string) => `{
  "main": "./contracts/${name}.${syntax}",
  "deploy": "./build/contracts/${name}.tz",
  "storage": "./storages/InitialStorage",
  "module": "Counter",
  "projectName": "${projectName}"
}
`;

export const getExamples = (
  name: string,
  template: string,
  projectName: string,
  syntax: string
): { [a: string]: { name: string; content: string } | undefined } => {
  if (template === "increment") {
    return {
      contractM:
        syntax === "mligo"
          ? {
              name: `.workspaces/${name}/contracts/Contract.mligo`,
              content: incrementM,
            }
          : undefined,
      contractJ:
        syntax === "jsligo"
          ? {
              name: `.workspaces/${name}/contracts/Contract.jsligo`,
              content: incrementJ,
            }
          : undefined,

      testM:
        syntax === "mligo"
          ? {
              name: `.workspaces/${name}/tests/Counter.mligo`,
              content: testM,
            }
          : undefined,
      testJ:
        syntax === "jsligo"
          ? {
              name: `.workspaces/${name}/tests/Counter.jsligo`,
              content: testJ,
            }
          : undefined,

      storageM:
        syntax === "mligo"
          ? {
              name: `.workspaces/${name}/storages/InitialStorage`,
              content: incrementMStorage,
            }
          : undefined,
      storageJ:
        syntax === "jsligo"
          ? {
              name: `.workspaces/${name}/storages/InitialStorage`,
              content: incrementJStorage,
            }
          : undefined,

      readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
      config: {
        name: `.workspaces/${name}/config.json`,
        content: config("Contract", projectName, syntax),
      },
    };
  }

  return {
    contract: {
      name: `.workspaces/${name}/contracts/Contract.${syntax}`,
      content: "",
    },
    storage: {
      name: `.workspaces/${name}/storages/Storage.${syntax}`,
      content: "",
    },
    readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
    config: {
      name: `.workspaces/${name}/config.json`,
      content: config("Contract", projectName, syntax),
    },
  };
};
