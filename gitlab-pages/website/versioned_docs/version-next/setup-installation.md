---
id: version-next-setup-installation
title: Installation
original_id: setup-installation
---

There are currently two ways to get started with Ligo, both of those will allow you to use the Ligo CLI with your contracts. You can choose to use either the Docker image, or to compile & build the Ligo CLI yourself.

## Docker

> 🐳 You can find instructions on how to install Docker [here](https://docs.docker.com/install/).

Easiest way to use LIGO is through the Docker image available at [Docker Hub](https://hub.docker.com/r/stovelabs/granary-ligo). Sources for the image can be found on [Github](https://github.com/stove-labs/granary/blob/develop/docker/ligo/Dockerfile).
You can either run the docker image yourself, or you can setup a global ligo executable as shown below.

### Setting up a globally available `ligo` executable
```zsh
wget https://gitlab.com/maht0rz/ligo-documentation/blob/master/ligo-docker.sh && \
sudo cp ligo-docker.sh /usr/local/bin/ligo && \
sudo chmod +x /usr/local/bin/ligo && \
rm ligo-docker.sh
```

> ⚠️ Please note that the **current docker image is quite chunky**, it may take a while to download depending on your internet connection. 

**Verify your ligo installation by running:**
```zsh
ligo --help
```


## Manual installation

For now, please refer to the steps described in the [Dockerfile](https://github.com/stove-labs/granary/blob/develop/docker/ligo/Dockerfile).



