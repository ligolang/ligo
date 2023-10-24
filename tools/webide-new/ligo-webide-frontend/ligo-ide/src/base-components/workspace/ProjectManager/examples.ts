const incrementM = `type storage = int

type ret = operation list * storage

(* Three entrypoints *)

[@entry]
let increment (delta : int) (store : storage) : ret = [], store + delta

[@entry]
let decrement (delta : int) (store : storage) : ret = [], store - delta

[@entry]
let reset (() : unit) (_ : storage) : ret = [], 0
`;

const incrementJ = `type storage = int;
type ret = [list<operation>, storage];
// Three entrypoints

@entry
const increment = (delta: int, store: storage): ret =>
  [list([]), store + delta];
@entry
const decrement = (delta: int, store: storage): ret =>
  [list([]), store - delta];
@entry
const reset = (_p: unit, _s: storage): ret => [list([]), 0]
`;

const incrementMStorage = "0";

const incrementJStorage = "0";

const config = (name: string, projectName: string, syntax: string) => `{
  "main": "./contracts/${name}.${syntax}",
  "deploy": "./build/contracts/${name}.tz",
  "storage": "./storages/Storage.${syntax}",
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
              name: `.workspaces/${name}/contracts/Increment.mligo`,
              content: incrementM,
            }
          : undefined,
      contractJ:
        syntax === "jsligo"
          ? {
              name: `.workspaces/${name}/contracts/Increment.jsligo`,
              content: incrementJ,
            }
          : undefined,

      storageM:
        syntax === "mligo"
          ? {
              name: `.workspaces/${name}/storages/Storage.mligo`,
              content: incrementMStorage,
            }
          : undefined,
      storageJ:
        syntax === "jsligo"
          ? {
              name: `.workspaces/${name}/storages/Storage.jsligo`,
              content: incrementJStorage,
            }
          : undefined,

      readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
      config: {
        name: `.workspaces/${name}/config.json`,
        content: config("Increment", projectName, syntax),
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
