---
id: installation
title: Installation
---

There are currently two ways to get started with Ligo. You can choose to use a Docker image, or to install packages for your Debian Linux distribution.

## Dockerized installation (recommended)

> 🐳 You can find instructions on how to install Docker [here](https://docs.docker.com/install/).

It's easiest to use LIGO through one of its Docker images. You have two options:
* Use our installation script to set up a globally available LIGO
executable (see below). This manages the Docker bits for you. 
* Use the Docker image available at [Docker Hub](https://hub.docker.com/r/ligolang/ligo).
This lets you run multiple versions and keep your installation(s) self contained, but requires more familiarity with Docker.

Sources for the image can be found on [GitLab](https://gitlab.com/ligolang/ligo/blob/master/docker/Dockerfile).
If this is your first time using Docker, you probably want to set up a global LIGO executable as shown below.

### Setting up a globally available `ligo` executable

> You can install additional ligo versions by replacing `next` with the required version number

Download the latest binaries here: https://gitlab.com/ligolang/ligo/pipelines/85536879/builds or get the latest pre-release:

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


## Debian Linux package installation

We have produced .deb packages for a few Debian Linux versions. They will install a global `ligo` executable. 
First download one of the packages below, and then install using:

```
sudo apt install ./<package_name_here>.deb
```

- [Ubuntu 18.04](/deb/ligo_ubuntu-18.04.deb)
- [Ubuntu 19.04](/deb/ligo_ubuntu-19.04.deb)
- [Debian 9](/deb/ligo_debian-9.deb)
- [Debian 10](/deb/ligo_debian-10.deb)

## Release schedule

Important: LIGO is currently being released on a rolling release schedule. This means that you always get the latest development features. You can find our [rolling builds at the CI](https://gitlab.com/ligolang/ligo/pipelines).
