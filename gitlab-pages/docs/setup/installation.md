---
id: installation
title: Installation
---

There are currently two ways to get started with Ligo, both of those will allow you to use the Ligo CLI with your contracts. You can choose to use either a Docker image, or to compile & build the Ligo CLI yourself.

## Dockerized installation (recommended)

> 🐳 You can find instructions on how to install Docker [here](https://docs.docker.com/install/).

<<<<<<< HEAD
Easiest way to use LIGO is through the Docker image available at [Docker Hub](https://hub.docker.com/r/ligolang/ligo). Sources for the image can be found on [Gitlab](https://gitlab.com/ligolang/ligo/blob/dev/docker/Dockerfile).
You can either run the docker image yourself, or you can setup a global ligo executable as shown below.
=======
It's easiest to use LIGO through one of its Docker images. You have two options,
the first is to use our installation script to set up a globally available LIGO
executable (see below). This manages the Docker bits for you. The second
is to directly use the Docker image available at [Docker Hub](https://hub.docker.com/r/ligolang/ligo).
This lets you run multiple versions and keep your installation(s) self contained, but requires more familiarity with Docker.
Sources for the image can be found on [Gitlab](https://gitlab.com/ligolang/ligo/blob/master/docker/Dockerfile).
If this is your first time using Docker, you probably want to set up a global ligo executable as shown below.
>>>>>>> Edit installation instructions to emphasize that Docker newbs should use the install script

### Setting up a globally available `ligo` executable

> You can install additional ligo versions by replacing `next` with the required version number

```zsh
# next (pre-release)
curl https://gitlab.com/ligolang/ligo/raw/dev/scripts/installer.sh | bash -s "next"
```
<!--
```
# e.g. 1.0.0 (stable)
curl https://gitlab.com/ligolang/ligo/raw/master/scripts/installer.sh | bash -s "1.0.0"
```
-->

**Verify your ligo installation by running:**
```zsh
ligo --help
```


## Manual installation (advanced)

For now, please refer to the steps described in the [Dockerfile](https://gitlab.com/ligolang/ligo/blob/master/docker/Dockerfile).