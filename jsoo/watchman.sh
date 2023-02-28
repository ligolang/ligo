#! /bin/sh

watchman -j <<-EOT
["trigger", "../_build/default/src/bin", {
  "name": "cp-jsoo-js",
  "expression": ["suffix", "js"],
  "chdir": "$PWD",
  "command": ["npm", "run", "cp-static"]
}]
EOT
