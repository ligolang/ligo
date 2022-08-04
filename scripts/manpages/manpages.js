/**
 * Run this script from the root of the ligo repo
 * You should build the ligo project before running
 * this script.
 * ```sh
 * node ./scripts/manpages/manpages.js
 * ```
 * This will generate the markdown version of the
 * manpages in `gitlab-pages/docs/manpages` directory
 */

const args = process.argv.slice(2);

const fs = require("fs");
const { exec } = require("child_process");

const OUT_DIR = "./gitlab-pages/docs/manpages"

const LIGO = args[0] !== undefined ? args[0] : "./_build/install/default/bin/ligo"

const commands = [
    "compile constant",
    "compile contract",
    "compile expression",
    "compile parameter",
    "compile storage",

    "init contract",
    "init library",

    "run dry-run",
    "run evaluate-call",
    "run evaluate-expr",
    "run interpret",
    "run test",

    "info get-scope",
    "info list-declarations",
    "info measure-contract",

    "transpile contract",
    "transpile expression",

    "mutate ast",
    "mutate cst",

    "repl",

    "changelog",

    "print preprocessed",
    "print pretty",
    "print dependency-graph",
    "print cst",
    "print ast-imperative",
    "print ast-sugar",
    "print ast-core",
    "print ast-typed",
    "print ast-aggregated",
    "print mini-c",

    "install",
]                            
            
const TEMPLATE = ({ synopsis, descrption, flags }) => `
### SYNOPSIS
${synopsis}

### DESCRIPTION
${descrption}

### FLAGS
${flags.map(({ name, desc }) => `**${name}**\n${desc}\n`).join("\n")}

`

commands.map(command => {
    const cmd = `${LIGO} ${command} --help`;
    exec(cmd, (error, stdout, stderr) => {
        if (error) {
            console.log(`error: ${error.message}`);
            return;
        }
        if (stderr) {
            console.log(`stderr: ${stderr}`);
            return;
        }

        const lines = stdout.split("\n").filter(x => x != '')
        let i = 1
        if (lines[i].includes("Warning:")) {
            i += 1
        }
        const synopsis = lines[i].trim();
        const descrption = lines[i+1].trim();
        let [flags, last] = lines.slice(i+2)
                           .map(line => line.trim())
                           .reduce(([acc, sofar], line) => {
                               if (line.startsWith("[-")) {
                                    acc.push(sofar)
                                    sofar = []
                               }
                               sofar.push(line);
                               return [acc, sofar];
                           }, [[], []])
        flags.push(last);
        flags.shift();
        flags = flags.map(flag => {
            let [name, desc] = flag[0].split("]");
            name = name.slice(1)
            desc = desc.replace("... ", "")
            desc = [desc.trim(), ...flag.slice(1)]
            desc = desc.join(" ")
            return { name, desc }
        })

        const data = {
            synopsis,
            descrption,
            flags
        }
        
        const manpage = TEMPLATE(data) 
        
        fs.writeFileSync(`${OUT_DIR}/${command}.md`, manpage)

    })
})

const MAIN_COMMAND_TEMPLATE = ({ synopsis, descrption, subcommands }) => `
### SYNOPSIS
${synopsis}

### DESCRIPTION
${descrption}

### SUB-COMMANDS
${subcommands.map(({ name, desc }) => `**${name}**\n${desc}\n`).join("\n")}

`

exec(`${LIGO} --help`, (error, stdout, stderr) => {
    if (error) {
        console.log(`error: ${error.message}`);
        return;
    }
    if (stderr) {
        console.log(`stderr: ${stderr}`);
        return;
    }

    const lines = stdout.split("\n").filter(x => x != '')
    
    const descrption = lines[0].trim();
    const synopsis = lines[1].trim();
    const subcommands = lines.slice(3)
                             .map(subcommand => {
                                let [name, ...desc] = subcommand.trim().split(/\s+/)
                                desc = desc.join(" ")
                                return { name, desc }
                             })

    const data = { descrption, synopsis, subcommands }
                             
    const manpage = MAIN_COMMAND_TEMPLATE(data) 
    
    fs.writeFileSync(`${OUT_DIR}/ligo.md`, manpage)
})
