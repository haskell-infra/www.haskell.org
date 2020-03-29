---
title: Downloads for Windows
page: downloads
isDownloads: true
---

# Downloads for Windows

## Package-based install

The package based installers on Windows use [chocolatey](https://chocolatey.org).
To use them first [configure Chocolatey](https://chocolatey.org/install) on your machine.

### Setup new environment

Steps to setup ghc and cabal are given in the [ghc chocolatey](https://chocolatey.org/packages/ghc)

For new installations it is recommended to use `Haskell-Dev` which will also setup and configure `MSys2` for use with `GHC` and `Cabal`.

**If upgrading from `Haskell-Platform` please see instructions below.**

`Haskell-Dev` can be installed from Chocolatey as follows from an elevated command prompt:

```bash
choco install haskell-dev
refreshenv
```

Chocolatey has limited [non-administrative](https://chocolatey.org/docs/installation#non-administrative-install) install support for those that absolutely need it.

After this you can upgrade or install additional `ghc` independently (see below).

Read more about [haskell-dev](https://hub.zhox.com/posts/introducing-haskell-dev/) or about how to integrate with Cloud CI systems.

Packages are installed into `%ProgramData%\chocolatey\lib\ghc\` and `%ProgramData%\chocolatey\lib\cabal\`

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu).

### Upgrading from Haskell-Platforms

If moving from a `Haskell-Platform` based installation to `Chocolatey` you will need to clean your `cabal` configuration before uninstalling `Haskell-Platform`.

To do this run:
```bash
cabal user-config init -f
```

and proceed to uninstall `Haskell-Platform` before installing using Chocolatey otherwise the Chocolatey packages may not modify the Cabal configuration file.

Do not keep both `Haskell-Platform` and `Chocolatey` based set up at the same time as they modify the same global configuration.

### Upgrading/Downgrading GHC or Cabal

`GHC` or `Cabal` can be upgraded using the [Chocolatey upgrade](https://chocolatey.org/docs/commandsupgrade) command.

For `GHC` (which may or may not trigger a cabal upgraded based on the version constraints)

```bash
choco upgrade ghc
refreshenv
```

For just `cabal` which will never trigger a `ghc` upgrade.
```bash
choco upgrade cabal
refreshenv
```

For both `ghc` and `cabal`

```bash
choco upgrade ghc cabal
refreshenv
```

Downgrades can be performed by specifying the version number during installation. e.g:

```bash
choco install ghc --version 8.6.5
refreshenv
```

A list of versions available can be seen on the [ghc Chocolatey](https://chocolatey.org/packages/ghc) website or by
using the command

```bash
choco list ghc --all
```

### Installing multiple versions of GHC

Multiple versions of `GHC ` can be installed concurrently by using the `-m` switch:

```bash
choco install ghc --version 8.4.1
choco install ghc --version 8.2.2 -m
refreshenv
```

You can then select which one you want to use by using the full compiler name, e.g `ghc-8.4.1.exe`.
These can be passed along to `cabal` as well:

```bash
cabal build -w ghc-8.4.1.exe
cabal build -w ghc-8.2.2.exe
```