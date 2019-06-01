#! /bin/sh
sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y opam mercurial darcs make m4 gcc libev-dev libgmp-dev pkg-config libhidapi-dev
opam init --bare -a

