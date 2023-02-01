---
title: Downloads
page: downloads
isDownloads: true
---

<div class="bs-callout bs-callout-info" style="padding: 0px 0px 10px 20px">
  <p>
    <h4>Psssst!</h4>
    Looking to get started with Haskell? If so, check out the [Get Started](/get-started) page!
  </p>
</div>

# Downloads

## Recommended installation instructions

*for Linux, macOS, FreeBSD, Windows or WSL2*


* Use <a href="https://www.haskell.org/ghcup/" class="btn btn-xs btn-primary">GHCup</a> to install GHC, cabal-install, Stack and haskell-language-server

<br>

* * *

### Find out more about the Haskell toolchain

The Haskell toolchain consists of the following tools:

*   [GHC](https://www.haskell.org/ghc/): the Glasgow Haskell Compiler
*   [cabal-install](https://cabal.readthedocs.io): the Cabal installation tool for managing Haskell software
*   [Stack](https://docs.haskellstack.org): a cross-platform program for developing Haskell projects
*   [haskell-language-server](https://github.com/haskell/haskell-language-server) (optional): A language server for developers to integrate with their editor/IDE

### Installation via native OS package manager

Alternatively, many operating systems provide GHC, cabal and Stack through their native package manager.  The packages are often out-of-date but if you prefer to use this method of installation then you will find useful links below.

<p><a data-toggle="collapse" href="#collapse-linux" class="btn btn-xs">Show Linux distros</a></p>

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

<p><a data-toggle="collapse" href="#collapse-nixos" class="btn btn-xs">Show Nix/NixOS instructions</a></p>

<div id="collapse-nixos" class="collapse">
<!-- This installation method is owned by @maralorn -->

*[Nix] is the package manager for NixOS and can also be used on other Linux distros and macOS.*

[Nix]: https://nixos.org

* **Using ghcup on NixOS does not work.** Use the official packages instead.
* You can get started by installing
    [GHC](https://search.nixos.org/packages?show=ghc&type=packages&query=ghc),
    [Haskell Language Server](https://search.nixos.org/packages?show=haskell-language-server&type=packages&query=haskell-language-server) and
    [cabal-install](https://search.nixos.org/packages?show=cabal-install&type=packages&query=cabal-install) (or
    [Stack](https://search.nixos.org/packages?show=stack&type=packages&query=stack)) via Nix.
* Read the [documentation](https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-create-a-development-environment) to learn more about development environments and packaging for Haskell with Nix.

</div>

<p><a data-toggle="collapse" href="#collapse-freebsd" class="btn btn-xs">Show FreeBSD packages</a></p>

<div id="collapse-freebsd" class="collapse">

  * Ports:
  [GHC](https://www.freshports.org/lang/ghc/),
  [cabal](https://www.freshports.org/devel/hs-cabal-install/),
  [Stack](https://www.freshports.org/devel/stack/)
</div>

<p><a data-toggle="collapse" href="#collapse-windows" class="btn btn-xs">Show Windows packages</a></p>

<div id="collapse-windows" class="collapse">

* [Chocolatey](https://chocolatey.org/install):
  [GHC](https://community.chocolatey.org/packages/ghc),
  [cabal](https://community.chocolatey.org/packages/cabal),
  [Stack](https://community.chocolatey.org/packages/haskell-stack)
</div>
