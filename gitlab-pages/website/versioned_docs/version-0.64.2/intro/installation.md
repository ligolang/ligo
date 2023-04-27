---
id: installation
title: Installation
---

Currently Ligo can be installed on Linux and MacOS. It still possible to use it on Windows through WSL or docker.

You can also try LIGO in a Gitpod environment

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://gitlab.com/ligolang/template-ligo)


## Static Linux binary

The `ligo` executable is statically linked. It should run on most modern Linux distributions.

You can get the rolling release [here](https://gitlab.com/ligolang/ligo/-/jobs/4190942182/artifacts/raw/ligo), make it executable, and you are done!

```zsh
wget https://gitlab.com/ligolang/ligo/-/jobs/4190942182/artifacts/raw/ligo
chmod +x ./ligo
```

For a specific version, you can visit our [release page](https://gitlab.com/ligolang/ligo/-/releases/).  
Optionally, you can put it somewhere in your `PATH` for easy access:

```zsh
sudo cp ./ligo /usr/local/bin
```

## MacOS

Try our tap,

```
brew tap ligolang/ligo https://gitlab.com/ligolang/ligo.git
brew install ligolang/ligo/ligo
```

## Debian Linux package installation

A `.deb` package containing the static `ligo` executable is also available.
First, download [the package](https://gitlab.com/ligolang/ligo/-/jobs/4190942182/artifacts/raw/ligo.deb), and then install using: 

```zsh
sudo apt install ./ligo.deb
```

## Arch User repository

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

## Dockerised installation
If you've [installed 🐳 Docker](https://docs.docker.com/install/), you can run the latest [LIGO release ](./changelog.md):

Linux or OSX:
> ```sh
> docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.64.2
> ```
> For convenience you can alias the above command
> ```sh
> alias ligo='docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.64.2'
> ```
> To make this `alias` persistent across terminal sessions you need to configure your shell.     
> Here is a [good link](https://www.tecmint.com/create-alias-in-linux/) with the steps on how to do that.

Windows:
> ```dos
> docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:0.64.2`
> ```
> For convenience you can alias the above command
> ```dos
> doskey ligo=docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:0.64.2 $*
> ```
> To make the alias persistent across terminal sessions you need to add the `doskey` to the Windows Registry.  
> Follow [this stackoverflow answer](https://stackoverflow.com/a/21040825) for the steps on how to do that.

Or if you want the development version, replace the version above with `next`.

Or run one of the older versions found on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

## Windows

> **Disclaimer**
>
> Windows version is beta quality and is still working in progress. Please report any issues found
> and feature requests.

### via NPM
Windows users can install LIGO via NPM.

```sh
npm i -g ligolang@windows-beta
```

### via GUI installer

You can download the installer from [here hosted on our Gitlab](https://gitlab.com/api/v4/projects/12294987/packages/generic/ligo_windows/current/ligo_installer_beta.exe).

#### Prerequisite

The installer use `nodejs` you will need to [install](https://nodejs.org/en/download/) it if it's not already done. 

#### Working around the "unrecognised app" warning screen

The installer hasn't been signed with a recognized Windows Developer ID. So for now, you might see the following.
![Unrecognised App Warning](/img/windows-unrecognised.png)

For now, you'll have to click on `More info`, and then, `Run anyway`.
![Run Anyway](/img/windows-more-info-clicked.png)



## Releases

Releases are available at the [releases page of GitLab project](https://gitlab.com/ligolang/ligo/-/releases). All the artifacts are attached there.

If you wish to see the changelog, you can either run `ligo changelog` or go to [this page](https://ligolang.org/docs/next/intro/changelog). It contains links to corresponding releases, should you wish to download the artifacts.
