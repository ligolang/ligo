#! /bin/sh

WATCHED_DIR="../_build/default/src/bin"
watchman watch $WATCHED_DIR
watchman -j <<-EOT
["trigger", "$WATCHED_DIR", {
  "name": "cp-jsoo-js",
  "expression": ["suffix", "js"],
  "chdir": "$PWD",
  "command": ["npm", "run", "cp-static"]
}]
EOT
