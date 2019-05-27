#!/bin/bash
# You can run this installer like this:
# curl https://gitlab.com/ligolang/ligo/blob/master/scripts/installer.sh | bash
# Make sure the marigold/ligo image is published at docker hub first 
set -euET -o pipefail
echo "Installing LIGO"

# Install the ligo.sh from master
wget https://gitlab.com/ligolang/ligo/blob/master/scripts/ligo.sh

# Copy the exucutable to the appropriate directory
sudo cp ligo.sh /usr/local/bin/ligo
sudo chmod +x /usr/local/bin/ligo
rm ligo.sh

# Pull the docker image used by ligo.sh
docker pull ligolang/ligo:latest

# Installation finished, try running 'ligo' from your CLI
echo "Installation successful, try to run 'ligo --help' now. \n"