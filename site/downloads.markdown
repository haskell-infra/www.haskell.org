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

## Recommended installation instructions { #ghcup }

*for Linux, macOS, FreeBSD, Windows or WSL2*

1. Install GHC, cabal-install and haskell-language-server via [GHCup](https://www.haskell.org/ghcup/)
2. To install stack, follow the instructions [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/) *(N.B. stack does not support FreeBSD)*

* * *

### Native OS packages { #distro-packages }

<p><a data-toggle="collapse" href="#collapse-linux" class="btn btn-xs btn-primary">Show Linux distros</a></p>

<div id="collapse-linux" class="collapse">

##### <span style="text-decoration: underline">Ubuntu</span>

* official packages
  - [GHC](https://packages.ubuntu.com/search?keywords=ghc&searchon=names&suite=all&section=all)
  - [cabal](https://packages.ubuntu.com/search?suite=all&section=all&arch=any&keywords=cabal-install&searchon=names)
  - [stack](https://packages.ubuntu.com/search?suite=all&section=all&arch=any&keywords=haskell-stack&searchon=names)
* [3rd party ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc)

##### <span style="text-decoration: underline">Debian</span>

* official packages
  - [GHC](https://packages.debian.org/search?keywords=ghc&searchon=names&suite=all&section=all)
  - [cabal](https://packages.debian.org/search?suite=all&section=all&arch=any&searchon=names&keywords=cabal-install)
  - [stack](https://packages.debian.org/search?suite=all&section=all&arch=any&searchon=names&keywords=haskell-stack)

##### <span style="text-decoration: underline">Fedora</span>

* official packages
  - [GHC](https://packages.fedoraproject.org/pkgs/ghc/ghc/)
  - [cabal](https://packages.fedoraproject.org/pkgs/cabal-install/cabal-install/)
  - [stack](https://packages.fedoraproject.org/pkgs/haskell-platform/stack/)
 
##### <span style="text-decoration: underline">openSUSE</span>

* official packages
  - [GHC](https://software.opensuse.org/package/ghc?search_term=%22ghc%22)
  - [cabal](https://software.opensuse.org/package/cabal-install)
* [devel:languages:haskell repo](https://build.opensuse.org/project/show/devel:languages:haskell)

##### <span style="text-decoration: underline">Gentoo</span>

* official packages
  - [GHC](https://packages.gentoo.org/packages/dev-lang/ghc)
  - [cabal](https://packages.gentoo.org/packages/dev-haskell/cabal)
  - [stack](https://packages.gentoo.org/packages/dev-haskell/stack)
* [3rd party repo](https://github.com/gentoo-haskell/gentoo-haskell)

##### <span style="text-decoration: underline">Arch Linux</span>

**DO NOT USE ARCH LINUX PROVIDED GHC PACKAGES. THEY ARE BROKEN.**

</div>

<p><a data-toggle="collapse" href="#collapse-freebsd" class="btn btn-xs btn-primary">Show FreeBSD packages</a></p>

<div id="collapse-freebsd" class="collapse">
* ports
  - [GHC](https://www.freshports.org/lang/ghc/)
  - [cabal](https://www.freshports.org/devel/hs-cabal-install/)
  - [stack](https://www.freshports.org/devel/stack/)
</div>

<p><a data-toggle="collapse" href="#collapse-windows" class="btn btn-xs btn-primary">Show Windows packages</a></p>

<div id="collapse-windows" class="collapse">
* [Chocolatey](https://chocolatey.org/install) packages
  - [GHC](https://community.chocolatey.org/packages/ghc)
  - [cabal](https://community.chocolatey.org/packages/cabal)
  - [stack](https://community.chocolatey.org/packages/haskell-stack)
</div>

