---
title: Downloads
page: downloads
isDownloads: true
---

# Downloads

## Haskell Platform

### What it is

<a name="platform"></a>The Haskell Platform is a self-contained, all-in-one installer. After download, you will have everything necessary to build Haskell programs against a core set of useful libraries. It comes in both core versions with tools but no libraries outside of GHC core, or full versions, which include a broader set of globally installed libraries.

### What you get

*   The [Glasgow Haskell Compiler](https://www.haskell.org/ghc)
*   The [Cabal build system](https://www.haskell.org/cabal/), which can install new packages, and by default fetches from [Hackage](https://hackage.haskell.org/), the central Haskell package repository.
*   the [Stack](http://docs.haskellstack.org) tool for developing projects
*   Support for profiling and code coverage analysis
*   35 core & widely-used [packages](https://www.haskell.org/platform/contents.html)

### How to get it

The Platform is provided as a single installer, and can be downloaded at the links below.

*   [Linux](http://www.haskell.org/platform/linux.html)
*   [OS X](http://www.haskell.org/platform/mac.html)
*   [Windows](http://www.haskell.org/platform/windows.html)

* * *

### Haskell IDEs & other distributions

In addition to the generic, cross-platform Haskell toolchain described above, there are also easy-to-use, platform-specific distributions and IDEs. The Haskell Wiki contains a [list of the most popular ones](https://wiki.haskell.org/Distributions).


* * *

## Advanced Installation with Stack

These options make different choices as to what is installed globally on your system and what is maintained in project-specific environments. Global installations allow more sharing across users and projects, but at the cost of potential conflicts between projects. To avoid these conflicts, each option has a lightweight _sandboxing_ feature that creates largely self-contained, per-project environments. With Minimal you can optionally sandbox the libraries, avoiding most conflicts. Stack sandboxes the compiler, tools and libraries, so avoids nearly all kinds of conflicts between projects. With Platform you can also optionally sandbox libraries, but not the globally installed platform libraries.

### What it is<a name="stack"></a>

Stack is a cross-platform build tool for Haskell that handles management of the toolchain (including the GHC compiler and MSYS2 on Windows), building and registering libraries, and more.

### What you get

*   Once downloaded, it has the capacity to download and install GHC and other core tools.
*   Project development is isolated within sandboxes, including automatic download of the right version of GHC for a given project.
*   It manages all Haskell-related dependencies, ensuring reproducible builds.
*   It fetches from a curated repository of over a thousand packages by default, known to be mutually compatible.
*   It can optionally use Docker to produce standalone deployments.

### How to get it

The [install and upgrade page](http://docs.haskellstack.org/en/stable/install_and_upgrade/) describes how to download Stack on various platforms, although the main three are repeated here:

*   [Ubuntu Linux](http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu)
*   [OS X](http://docs.haskellstack.org/en/stable/install_and_upgrade/#os-x)
*   [Windows](http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)

Instructions for other Linux distributions, including Debian, Fedora, Red Hat, Nix OS, and Arch Linux, are also available.

### Where to get help<a name="stackhelp"></a>

For help with Haskell and GHC in general, see the links mentioned [above](#help). For Stack itself there are also the following resources:

*   The [README](https://github.com/commercialhaskell/stack/#readme) offers a general overview, and help with installation.
*   There is an [in-depth guide](http://docs.haskellstack.org) to using Stack.
*   [Getting started with Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html) introduces how to build new projects using Stack.
*   You may post issues and feature requests on its [GitHub issue tracker](https://github.com/commercialhaskell/stack).
*   There is a [mailing list for Stack](https://groups.google.com/d/forum/haskell-stack)
*   There is a dedicated [#haskell-stack IRC channel](irc://irc.freenode.net/haskell-stack) on the Freenode IRC network.
*   The [StackOverflow haskell-stack tag](http://stackoverflow.com/questions/tagged/haskell-stack) has many stack-specific questions and answers.

## Additional Libraries<a name="libraries"></a>

In Haskell, packages are configured and built with the Cabal package system built into GHC (and other compilers). For more specific details, see [The Cabal User Guide](https://www.haskell.org/cabal/users-guide/). The command line tools to download and install packages are either `cabal` or `stack`, each having different workflows. For details on their usage, see the documentation above.

### Hackage

Hackage is a repository of packages to which anyone can freely upload at any time. The packages are available immediately and documentation will be generated and hosted there. It can be used by cabal install.

You can install a package using cabal by running:

```
$$ cabal update
$$ cabal install the-package
```

Note that if you are not in a sandbox, this will install the package globally, which is often not what you want, so it is recommended to set up sandboxes in your project directory by running `cabal sandbox init`.

[Go to Hackage →](https://hackage.haskell.org/packages/)

### LTS Haskell

LTS Haskell is a stackage-based long-term support set of packages which build and pass tests together, with backported bug fixes.

[Get LTS Haskell →](http://www.stackage.org/lts)

### Stackage Nightly

Stackage is a nightly generated stable repository of snapshots of package sets in which only packages which build and pass tests together are bundled together into a snapshot.

[Get Stackage Nightly →](http://www.stackage.org/nightly)

### From source control repositories

Installing from a source repository is also possible. For example, to clone and install the network package from source, you would run:

```
$$ git clone https://github.com/haskell/network
$$ cabal install network
```

Or:

```
$$ git clone https://github.com/haskell/network
$$ cd network
$$ cabal install
```
# You need Help?

### Where to get help<a name="help"></a>

*   For help learning Haskell itself, start with the [Documentation](https://www.haskell.org/documentation) page on the [Haskell Wiki](https://wiki.haskell.org/).
*   If you need help with [GHC](https://www.haskell.org/ghc)---the Haskell compiler---there is a comprehensive [GHC User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html).
*   For help using Cabal to download or create additional packages (see [below](#libraries)), there is the [Cabal User Guide](https://www.haskell.org/cabal/users-guide/).
*   For help using Stack to download or create packages, see the stack documentation [below](#stackhelp).
*   Finally, you can ask questions of other Haskell users and experts on the [#haskell IRC channel](irc://irc.freenode.net/haskell) on the Freenode IRC network.
