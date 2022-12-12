#! /bin/sh

set -o xtrace

new_section () {
    set +o xtrace
    echo ""
    echo ""
    echo "========"
    echo $1
    set -o xtrace
}

REGISTRY_URL=http://localhost:4873

if ! command -v verdaccio &> /dev/null
then
    new_section "Installing verdaccio"
    yarn global add verdaccio
fi

VERDACCIO_PID=
if ! lsof -i :4873 &> /dev/null;
then
    new_section "Setting up verdaccio"
    mkdir -p ~/.config/verdaccio ~/.local/share/verdaccio/storage
    cp ./.ci/verdaccio-config.yaml ~/.config/verdaccio/config.yaml
    verdaccio&
    VERDACCIO_PID="$!"
    sleep 1
fi

new_section "Packaging for NPM"
node scripts/package.js 
new_section "Publishing to local NPM"
tar -xf $PWD/package.tar.gz
npm publish --registry $REGISTRY_URL $PWD/package

cd esy-test/
export ESY__PREFIX=$HOME/_esy_test/prefix
rm -rf $ESY__PREFIX
mkdir -p $ESY__PREFIX
esy i --npm-registry $REGISTRY_URL
esy b
kill "$VERDACCIO_PID"
