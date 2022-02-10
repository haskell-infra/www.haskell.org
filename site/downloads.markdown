---
title: Downloads
page: downloads
isDownloads: true
---

# Downloads

This page describes the installation of the Haskell toolchain, which consists of the following tools:

*   [GHC](https://www.haskell.org/ghc/): the Glasgow Haskell Compiler

*   [cabal-install](https://cabal.readthedocs.io): the Cabal installation tool for managing Haskell software

*   [stack](https://docs.haskellstack.org): a cross-platform program for developing Haskell projects

*   [haskell-language-server](https://github.com/haskell/haskell-language-server) (optional): A language server for developers to integrate with their editor/IDE

## Installation instructions { #ghcup }

*for Linux, macOS, FreeBSD, Windows or WSL2*

1. Install GHC, cabal-install and haskell-language-server via [GHCup](https://www.haskell.org/ghcup/)
2. To install stack, follow the instructions [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/) *(N.B. stack does not support FreeBSD)*

* * *

## Alternative installation options { #ghc-install-manual }

*   [Using a package manager](#package-manager)

*   [Official bindists](#bindists)

*   [Building from source](#from-source)

*   [Other options](#other-options)

### Using a package manager { #package-manager }

#### Linux

Refer to your distribution package manager documentation. For convenience, below are a few distribution specific instructions, outlining 3rd party repository use as well.

*Note: Most Linux distros don't allow different versions of GHC to be installed in parallel. Sometimes the GHC packages are a bit outdated as well. In either case, consider using [GHCup](#ghcup) instead.*

*Note: haskell-language-server isn't packaged by most distributions. Instead you will have to install it manually, see the [release page](https://github.com/haskell/haskell-language-server/releases), unless you use VSCode, which bootstraps everything automatically. This is optional.*

<p><a data-toggle="collapse" href="#collapse-linux" class="btn btn-xs btn-primary">Show Linux distros</a></p>

<div id="collapse-linux" class="collapse">
##### <span style="text-decoration: underline">Ubuntu</span>

Steps to setup ghc and cabal are given in the [ghc ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc)

Packages from the PPA can be installed as follows:
```bash
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-XXX ghc-YYY
```

Packages are installed into `/opt/ghc/bin` and `/opt/cabal/bin`

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu).

##### <span style="text-decoration: underline">Debian</span>

Steps to setup ghc and cabal are given in the [ghc debian apt repository](https://downloads.haskell.org/~debian/)

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#debian).

##### <span style="text-decoration: underline">Fedora</span>

GHC, cabal-install and stack are in the official Fedora repos, to install:

`sudo dnf install ghc cabal-install`

There are also Fedora module streams with newer versions of ghc:

```
sudo dnf module list ghc
sudo dnf module install ghc:X.Y
```

The different versions cannot be parallel installed.

There are also unofficial Fedora Copr repos with more recent [cabal-install](http://copr.fedorainfracloud.org/coprs/petersen/cabal-install).

##### <span style="text-decoration: underline">EPEL for RHEL/CentOS/etc</span>

*   EPEL 7 has ghc-7.6.3 and cabal-install-1.16.1.0
*   EPEL 5 and 6 have ghc-7.0.4 and cabal-install-0.10.2

To install these older versions of ghc and cabal-install from the official EPEL repo, just run the install command:

`sudo yum install ghc cabal-install`

For newer versions of ghc you can use the unofficial Fedora Copr repos:

*   [petersen/ghc-8.6.5 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.6.5/)
*   [petersen/ghc-8.4.4 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.4.4/)
*   [petersen/ghc-8.2.2 Copr repo (EL7,EL6)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.2.2/)  
*   [petersen/ghc-8.0.2 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.0.2)

Note that the different ghc package versions cannot be installed in parallel.

##### <span style="text-decoration: underline">Arch Linux</span>

*Note: installing the Haskell toolchain via the Arch Linux package manager is not recommended, since it enforces dynamic linking, which can cause various issues. Instead consider [GHCup](#ghcup).*

The official repos on Arch Linux contain packages `ghc`, `cabal-install`, `happy`, `alex`, `haddock`. Install them with:

<pre>sudo pacman -S ghc cabal-install happy alex haskell-haddock-library</pre>
 
##### <span style="text-decoration: underline">openSUSE Leap</span>

*   Leap 15.1 has ghc-8.6.4 and cabal-install-2.4.0.0

To install from official openSUSE:Leap repo, just run the install command:

`sudo zypper in ghc cabal-install`

For last stable version you can use the development openSUSE repository:

* [devel:languages:haskell repo](https://build.opensuse.org/project/show/devel:languages:haskell)

Use this command to add repository to your system:

`sudo zypper ar -f -p 90 https://download.opensuse.org/repositories/devel:/languages:/haskell/openSUSE_Leap_15.1/devel:languages:haskell.repo`

##### <span style="text-decoration: underline">openSUSE Tumbleweed</span>

*   Tumbleweed has last stable version of ghc and cabal install.

To install from official openSUSE:Tumbleweed repo, just run the install command:

`sudo zypper in ghc cabal-install`

##### <span style="text-decoration: underline">Gentoo</span>

While the Haskell toolchain is available in the main gentoo repository, it is recommended
to use the more up-to-date [gentoo-haskell](https://github.com/gentoo-haskell/gentoo-haskell) overlay. This can be done using layman:

```
sudo layman -a haskell
sudo emerge --ask dev-lang/ghc dev-haskell/cabal-install
```

</div>

#### Windows

1. [Configure Chocolatey](https://chocolatey.org/install) on your machine
2. At an elevated command prompt, run `choco install haskell-dev haskell-stack`, followed by `refreshenv`.

### Official bindists { #bindists }

GHC bindists are binary packages built as part of the GHC release process and are guaranteed to have passed the test suite. The installation process is a bit manual, so this is meant for power users.

The [GHC download page](https://www.haskell.org/ghc/download.html) gives an extensive overview of available options. Navigate to the version you seek and click on **Binary packages**. You may also visit [downloads.haskell.org/~ghc](https://downloads.haskell.org/~ghc/) for the complete list. Likewise, to install cabal-install manually visit [downloads.haskell.org/~cabal](https://downloads.haskell.org/~cabal/cabal-install-latest/). Stack binaries can be found on [github.com/commercialhaskell/stack](https://github.com/commercialhaskell/stack/releases).

Haskell-language-server binaries can be found [here](https://github.com/haskell/haskell-language-server/releases/).

### Building from source { #from-source }

Building from source can be a difficult task, but may be necessary if all other installation options fail. Refer to the [Haskell gitlab wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/#building-and-porting-ghc) for further instructions.

For building cabal-install from source, follow the instructions in the [README](https://github.com/haskell/cabal/blob/master/README.md).

For building haskell-language-server from source, follow the instructions [here](https://github.com/haskell/haskell-language-server#installation-from-source).

### Other options { #other-options }

Other popular installation options not further described here include:

* [nix](https://nixos.org/): a popular cross-distro package manager, aiming to provide reproducible builds and declarative configuration
