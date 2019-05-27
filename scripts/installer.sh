#!/bin/bash
# You can run this installer like this:
# curl https://gitlab.com/gabriel.alfour/ligo/blob/master/scripts/installer.sh | bash
# Make sure the marigold/ligo image is published at docker hub first 
set -euET -o pipefail
echo "Installing LIGO"

# Install the ligo.sh from master
wget https://gitlab.com/gabriel.alfour/ligo/blob/master/scripts/ligo.sh

# Copy the exucutable to the appropriate directory
sudo cp ligo.sh /usr/local/bin/ligo
sudo chmod +x /usr/local/bin/ligo
rm ligo.sh

# Installation finished, try running 'ligo' from your CLI
echo "Installation successful, try running 'ligo' now. \n"
