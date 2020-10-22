---
id: installation
title: Installation
---

There are currently three ways to get started with LIGO. You can choose to use a Docker image, a static Linux binary or to install packages for your Debian Linux distribution.

## Dockerized installation (recommended)

> 🐳 You can find instructions on how to install Docker [here](https://docs.docker.com/install/).

It's easiest to use LIGO through one of its Docker images. You have two options:
* Use our installation script to set up a globally available `ligo`
executable (see below). This manages the Docker bits for you. 
* Use the Docker image available at [Docker Hub](https://hub.docker.com/r/ligolang/ligo).
This lets you run multiple versions and keep your installation(s) self-contained but requires more familiarity with Docker.

Sources for the image can be found on [GitLab](https://gitlab.com/ligolang/ligo/blob/master/docker/Dockerfile).
If this is your first time using Docker, you probably want to set up a global `ligo` executable as shown below.

### Setting up a globally available `ligo` executable

<!--
> You can install additional LIGO versions by replacing `next` with the desired version number
-->

Get the latest pre-release:

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

**Verify your LIGO installation by running:**
```zsh
ligo --help
```

## Static Linux binary

The `ligo` executable is statically linked. It should run on most modern Linux distributions.

To use it, get it [here](https://ligolang.org/bin/linux/ligo), make it executable, you're done!

```zsh
wget https://ligolang.org/bin/linux/ligo
chmod +x ./ligo
```

Optionally, you can put it somewhere in your `PATH` for easy access:

```zsh
sudo cp ./ligo /usr/local/bin
```

## Debian Linux package installation

A `.deb` package containing the static `ligo` executable is also available.
First, download [the package](https://ligolang.org/deb/ligo.deb), and then install using: 

```zsh
sudo apt install ./ligo.deb
```

## Releases

Releases are available at the [releases page of gitlab project](https://gitlab.com/ligolang/ligo/-/releases). All the artifacts are attached there.

If you wish to see the changelog, you can either run `ligo changelog` or go to [this page](./changelog.md). It contains links to corresponding releases, should you wish to download the artifacts.
