---
id: installation
title: Installation
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

Currently Ligo can be installed on Linux and MacOS. It still possible to use it on Windows through WSL or docker.

You can also try LIGO in a Gitpod environment

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://gitlab.com/ligolang/template-ligo)

## Releases

Releases are available at the [releases page of GitLab project](https://gitlab.com/ligolang/ligo/-/releases). All the artifacts are attached there.

If you wish to see the changelog, you can either run `ligo changelog` or go to [this page](https://ligolang.org/docs/next/intro/changelog). It contains links to corresponding releases, should you wish to download the artifacts.

## Install
<Tabs
  defaultValue="Lbinary"
  values={[
    { label: 'Linux binary', value: 'Lbinary' },
    { label: 'MacOS', value: 'macos' },
    { label: 'Debian package', value: 'deb' },
    { label: 'Arch', value: 'AUR' },
    { label: 'Dockerized', value: 'docker' },
    { label: 'Windows', value: 'windows' }
  ]}>
<TabItem value="Lbinary">

The `ligo` executable is statically linked. It should run on most modern Linux distributions.

You can get the rolling release [here](https://gitlab.com/ligolang/ligo/-/jobs/5419828800/artifacts/raw/ligo), make it executable, and you are done!

```zsh
wget https://gitlab.com/ligolang/ligo/-/jobs/5419828800/artifacts/raw/ligo
chmod +x ./ligo
```

For a specific version, you can visit our [release page](https://gitlab.com/ligolang/ligo/-/releases/).  
Optionally, you can put it somewhere in your `PATH` for easy access:

```zsh
sudo cp ./ligo /usr/local/bin
```
</TabItem>
<TabItem value="macos">

Try our tap :

```bash
brew tap ligolang/ligo https://gitlab.com/ligolang/ligo.git
brew install ligolang/ligo/ligo
```

To upgrade ligo :

```bash
brew update
brew upgrade ligolang/ligo/ligo
```

</TabItem>
<TabItem value="deb">

A `.deb` package containing the static `ligo` executable is also available.
First, download [the package](https://gitlab.com/ligolang/ligo/-/jobs/5419828800/artifacts/raw/ligo.deb), and then install using: 

```zsh
sudo apt install ./ligo.deb
```
</TabItem>
<TabItem value="AUR">

It's possible to install latest release of ligo through [AUR](https://aur.archlinux.org/packages/ligo-bin)

```zsh
git clone https://aur.archlinux.org/ligo-bin.git
cd ligo-bin
makepkg -si
```

Or through tools like [yay](https://github.com/Jguer/yay)

```zsh
yay -S ligo-bin
```

</TabItem>
<TabItem value="docker">

If you've [installed 🐳 Docker](https://docs.docker.com/install/), you can run the latest [LIGO release](./changelog.md):

Linux or OSX:
> ```sh
> docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:1.1.0
> ```
> For convenience you can alias the above command
> ```sh
> alias ligo='docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:1.1.0'
> ```
> To make this `alias` persistent across terminal sessions you need to configure your shell.     
> Here is a [good link](https://www.tecmint.com/create-alias-in-linux/) with the steps on how to do that.

Or if you want the development version, replace the version above with `next`.

Or run one of the older versions found on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

</TabItem>
<TabItem value="windows">

It's possible to use ligo in [Docker](https://docs.docker.com/install/) through docker or [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install)

> ```dos
> docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:1.1.0`
> ```
> For convenience you can alias the above command
> ```dos
> doskey ligo=docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:1.1.0 $*
> ```
> To make the alias persistent across terminal sessions you need to add the `doskey` to the Windows Registry.  
> Follow [this stackoverflow answer](https://stackoverflow.com/a/21040825) for the steps on how to do that.

Or if you want the development version, replace the version above with `next`.

Or run one of the older versions found on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

</TabItem>
</Tabs>
