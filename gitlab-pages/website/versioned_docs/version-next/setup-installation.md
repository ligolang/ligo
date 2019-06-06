---
id: version-next-setup-installation
title: Installation
original_id: setup-installation
---

There are currently two ways to get started with Ligo, both of those will allow you to use the Ligo CLI with your contracts. You can choose to use either the Docker image, or to compile & build the Ligo CLI yourself.

## Dockerized installation (recommended)

> 🐳 You can find instructions on how to install Docker [here](https://docs.docker.com/install/).

Easiest way to use LIGO is through the Docker image available at [Docker Hub](https://hub.docker.com/r/ligolang/ligo). Sources for the image can be found on [Gitlab](https://gitlab.com/ligolang/ligo/blob/master/docker/Dockerfile).
You can either run the docker image yourself, or you can setup a global ligo executable as shown below.

### Setting up a globally available `ligo` executable

> You can install additional ligo versions by replacing `next` with the required version number

```zsh
# next (pre-release)
curl https://gitlab.com/ligolang/ligo/raw/dev/scripts/installer.sh | bash "next"

# e.g. 1.0.0 (stable)
curl https://gitlab.com/ligolang/ligo/raw/master/scripts/installer.sh | bash "1.0.0"
```

**Verify your ligo installation by running:**
```zsh
ligo --help
```


## Manual installation (advanced)

For now, please refer to the steps described in the [Dockerfile](https://gitlab.com/ligolang/ligo/blob/master/docker/Dockerfile).



