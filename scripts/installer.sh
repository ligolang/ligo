#!/bin/sh
set -e

# You can run this installer like this:
# curl https://gitlab.com/ligolang/ligo/blob/master/scripts/installer.sh | bash
# Make sure the marigold/ligo image is published at docker hub first

if test $# -ne 1; then
  printf 'Usage: path/to/installer.sh VERSION'\\n
  printf \\n
  printf '  where VERSION can be "next" or a version number like 1.0.0'\\n
  exit 1
else
  version=$1
  printf \\n'Installing LIGO (%s)'\\n\\n "$version"

  # Pull the docker image used by ligo.sh
  docker pull "ligolang/ligo:$version"

  # Install ligo.sh
  if test -d /usr/local/bin/ligo
  then
    echo "/usr/local/bin/ligo already exists and is a directory, cancelling installation"
  else
    sudo install -m 0755 /dev/stdin /usr/local/bin/ligo <<EOF
#!/bin/sh
set -e
if test "x\$PWD" = "x"; then
  echo "Cannot detect the current directory, the environment variable PWD is empty."
  exit 1
else
  docker run --rm -v "\$PWD":"\$PWD" -w "\$PWD" ligolang/ligo:$version "\$@"
fi
EOF
  fi
  # Installation finished, try running 'ligo' from your CLI
  printf \\n'Installation successful, try to run '\''ligo --help'\'' now.'\\n
fi
