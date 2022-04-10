---
title: Downloads
page: downloads
isDownloads: true
---

# Downloads

This page describes the installation of the Haskell toolchain, which consists of the following tools:

*   [GHC](https://www.haskell.org/ghc/): the Glasgow Haskell Compiler
*   [cabal-install](https://cabal.readthedocs.io): the Cabal installation tool for managing Haskell software
*   [Stack](https://docs.haskellstack.org): a cross-platform program for developing Haskell projects
*   [haskell-language-server](https://github.com/haskell/haskell-language-server) (optional): A language server for developers to integrate with their editor/IDE

## Recommended installation instructions

*for Linux, macOS, FreeBSD, Windows or WSL2*

1. Install GHC, cabal-install and haskell-language-server via [GHCup](https://www.haskell.org/ghcup/)
2. To install Stack, follow the [Stack installation guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

* * *

### Via native OS package manager

Alternatively, many operating systems provide GHC, cabal and Stack through their native package manager.  The packages are often out-of-date but if you prefer to use this method of installation then you will find useful links below.

<p><a data-toggle="collapse" href="#collapse-linux" class="btn btn-xs btn-primary">Show Linux distros</a></p>

<div id="collapse-linux" class="collapse">

#### Ubuntu

* Official packages:
  [GHC](https://packages.ubuntu.com/search?keywords=ghc&searchon=names&suite=all&section=all),
  [cabal](https://packages.ubuntu.com/search?suite=all&section=all&arch=any&keywords=cabal-install&searchon=names),
  [Stack](https://packages.ubuntu.com/search?suite=all&section=all&arch=any&keywords=haskell-stack&searchon=names)
* [Third party PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc)

#### Debian

* Official packages:
  [GHC](https://packages.debian.org/search?keywords=ghc&searchon=names&suite=all&section=all),
  [cabal](https://packages.debian.org/search?suite=all&section=all&arch=any&searchon=names&keywords=cabal-install),
  [Stack](https://packages.debian.org/search?suite=all&section=all&arch=any&searchon=names&keywords=haskell-stack)

#### Fedora

* Official packages
  [GHC](https://packages.fedoraproject.org/pkgs/ghc/ghc/),
  [cabal](https://packages.fedoraproject.org/pkgs/cabal-install/cabal-install/),
  [Stack](https://packages.fedoraproject.org/pkgs/haskell-platform/stack/)

#### openSUSE

* Official packages
  [GHC](https://software.opensuse.org/package/ghc?search_term=%22ghc%22),
  [cabal](https://software.opensuse.org/package/cabal-install),
* [devel:languages:haskell repo](https://build.opensuse.org/project/show/devel:languages:haskell)

#### Gentoo

* Official packages
  [GHC](https://packages.gentoo.org/packages/dev-lang/ghc),
  [cabal](https://packages.gentoo.org/packages/dev-haskell/cabal),
  [Stack](https://packages.gentoo.org/packages/dev-haskell/stack)
* [Third party repo](https://github.com/gentoo-haskell/gentoo-haskell)

#### Arch

**Do not use the Haskell development tools provided by Arch, they are broken.** For more information see [[1]](https://dixonary.co.uk/blog/haskell/cabal-2020) [[2]](https://stackoverflow.com/questions/65643699/what-is-the-suggested-way-of-setting-up-haskell-on-archlinux/65644318#65644318).

</div>

<p><a data-toggle="collapse" href="#collapse-freebsd" class="btn btn-xs btn-primary">Show FreeBSD packages</a></p>

<div id="collapse-freebsd" class="collapse">

  * Ports:
  [GHC](https://www.freshports.org/lang/ghc/),
  [cabal](https://www.freshports.org/devel/hs-cabal-install/),
  [Stack](https://www.freshports.org/devel/stack/)
</div>

<p><a data-toggle="collapse" href="#collapse-windows" class="btn btn-xs btn-primary">Show Windows packages</a></p>

<div id="collapse-windows" class="collapse">

* [Chocolatey](https://chocolatey.org/install):
  [GHC](https://community.chocolatey.org/packages/ghc),
  [cabal](https://community.chocolatey.org/packages/cabal),
  [Stack](https://community.chocolatey.org/packages/haskell-stack)
</div>
