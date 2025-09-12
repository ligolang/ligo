---
id: installation
title: Installation
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

Currently LIGO can be installed on Linux and MacOS.
It is possible to use it on Windows through WSL or docker.

You can also try LIGO in a Gitpod environment by clicking this button:

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://gitlab.com/ligolang/template-ligo)

## Releases

Releases are available at the [releases page of GitLab project](https://gitlab.com/ligolang/ligo/-/releases). All the artifacts are attached there.

To see the changelog, you can either run `ligo changelog` or go to [LIGO Changelog](./changelog).
The changelog contains links to corresponding releases.

## Install
<Tabs
  defaultValue="Lbinary"
  values={[
    { label: 'MacOS', value: 'macos' },
    { label: 'Debian Linux', value: 'deb' },
    { label: 'Arch Linux', value: 'AUR' },
    { label: 'Docker', value: 'docker' },
    { label: 'Windows', value: 'windows' },
    { label: 'Linux binary', value: 'Lbinary' },
  ]}>
<TabItem value="macos">

Install LIGO with Homebrew:

```bash
brew tap ligolang/ligo https://gitlab.com/ligolang/ligo.git
brew install ligolang/ligo/ligo
```

To upgrade LIGO:

```bash
brew update
brew upgrade ligolang/ligo/ligo
```

</TabItem>
<TabItem value="deb">

A `.deb` package containing the static `ligo` executable is available.
First download [the package](https://gitlab.com/ligolang/ligo/-/jobs/11330445323/artifacts/raw/ligo.deb) and then install it with this command:

```bash
sudo apt install ./ligo.deb
```
</TabItem>
<TabItem value="AUR">

You can install the latest release of ligo through [AUR](https://aur.archlinux.org/packages/ligo-bin):

```bash
git clone https://aur.archlinux.org/ligo-bin.git
cd ligo-bin
makepkg -si
```

It is also available through tools like [yay](https://github.com/Jguer/yay):

```bash
yay -S ligo-bin
```

</TabItem>
<TabItem value="docker">

If you've [installed 🐳 Docker](https://docs.docker.com/install/), you can run the latest [LIGO release](./changelog.md) in a Docker container with this command:

Linux or MacOS:

```bash
docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:1.11.0
```

Windows:

```bash
docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:1.11.0
```

For convenience, you can alias the command:

Linux or MacOS:

```bash
alias ligo='docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:1.11.0'
```

Windows:

```dos
doskey ligo=docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:1.11.0 $*
```

Then you can use the alias to run LIGO commands, as in this command:

```bash
ligo compile contract myContract.jsligo
```

To make this alias persistent across terminal sessions you must configure your shell to remember it.
See [these instructions for Linux](https://www.tecmint.com/create-alias-in-linux/) or [this stackoverflow answer for Windows](https://stackoverflow.com/a/21040825).

To use the development version of LIGO, replace the version above (`1.10.0`) with `next`.

To run an older version of LIGO, you can get older Docker images on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

</TabItem>
<TabItem value="windows">

It's possible to use ligo in [Docker](https://docs.docker.com/install/) through docker or [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install).

```dos
docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:1.11.0
```

For convenience, you can alias the command:
```dos
doskey ligo=docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:1.11.0 $*
```

Then you can use the alias to run LIGO commands, as in this command:

```dos
ligo compile contract myContract.jsligo
```

To make this alias persistent across terminal sessions you must configure your shell to remember it.
See [this stackoverflow answer](https://stackoverflow.com/a/21040825).

To use the development version of LIGO, replace the version above (`1.10.0`) with `next`.

To run an older version of LIGO, you can get older Docker images on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

</TabItem>
<TabItem value="Lbinary">

The `ligo` executable is statically linked. It should run on most modern Linux distributions.

You can download the current release [here](https://gitlab.com/ligolang/ligo/-/jobs/11330445323/artifacts/raw/ligo), make it executable, and you are done!
Optionally, you can put it somewhere in your `PATH` for easy access.

```bash
wget https://gitlab.com/ligolang/ligo/-/jobs/11330445323/artifacts/raw/ligo
chmod +x ./ligo
sudo cp ./ligo /usr/local/bin
```

To download a specific version, visit the [release page](https://gitlab.com/ligolang/ligo/-/releases/), which provides packages for some Linux distributions and a static binary for all Linux distributions.

To upgrade, download a new file and replace the old one.

</TabItem>
</Tabs>

<!-- updated use of entry -->