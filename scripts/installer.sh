#!/bin/bash
# You can run this installer like this:
# curl https://gitlab.com/ligolang/ligo/blob/master/scripts/installer.sh | bash
# Make sure the marigold/ligo image is published at docker hub first 
set -euET -o pipefail
version=$1
printf "\nInstalling LIGO ($version)\n\n"

if [ $version = "next" ]
    then
        # Install the ligo.sh from master
        wget https://gitlab.com/ligolang/ligo/raw/dev/scripts/ligo.sh
    else
        # Install the ligo.sh from master
        wget https://gitlab.com/ligolang/ligo/raw/master/scripts/ligo.sh
fi


# Overwrite LIGO version in the executable
sed -i '' "s/latest/$version/g" ligo.sh

# Copy the exucutable to the appropriate directory
sudo cp ligo.sh /usr/local/bin/ligo
sudo chmod +x /usr/local/bin/ligo
rm ligo.sh

# Pull the docker image used by ligo.sh
docker pull "ligolang/ligo:$version"

# Installation finished, try running 'ligo' from your CLI
printf "\nInstallation successful, try to run 'ligo --help' now.\n"