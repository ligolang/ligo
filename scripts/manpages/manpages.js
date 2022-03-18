const fs = require("fs");
const { exec } = require("child_process");

const commands = [
    "compile constant",
    "compile contract",
    "compile expression",
    "compile parameter",
    "compile storage",

    "run dry-run",
    "run evaluate-call",
    "run evaluate-expr",
    "run interpret",
    "run test",

    "info get-scope",
    "info list-declarations",
    "info measure-contract",

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
    "print ast-combined",
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
    const cmd = `/home/melwyn95/projects/nuke/ligo/_build/install/default/bin/ligo ${command} --help`;
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
        
        fs.writeFileSync(`${command}.md`, manpage)

    })
})