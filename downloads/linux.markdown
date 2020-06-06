---
title: Downloads for Linux
page: downloads
isDownloads: true
---

# Downloads for Linux

## Package-based install

### Ubuntu

Steps to setup ghc and cabal are given in the [ghc ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc)

Packages from the PPA can be installed as follows:
```bash
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-XXX ghc-YYY
```

Packages are installed into `/opt/ghc/bin` and `/opt/cabal/bin`

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu).

### Debian (jessie and stretch)

Steps to setup ghc and cabal are given in the [ghc debian apt repository](https://downloads.haskell.org/~debian/)

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#debian).

### Fedora

GHC, cabal-install and stack are in the official Fedora repos, to install:

`sudo dnf install ghc cabal-install stack`

There are also Fedora module streams with newer versions of ghc:

`sudo dnf module list ghc`
`sudo dnf module install ghc:X.Y`

The different versions cannot be parallel installed.

There are also unofficial Fedora Copr repos with more recent [cabal-install](http://copr.fedorainfracloud.org/coprs/petersen/cabal-install) and [stack](http://copr.fedorainfracloud.org/coprs/petersen/stack-2.3).

Steps to setup stack are also [on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#fedora).

### EPEL for RHEL/CentOS/etc

*   EPEL 7 has ghc-7.6.3 and cabal-install-1.16.1.0
*   EPEL 5 and 6 have ghc-7.0.4 and cabal-install-0.10.2

To install these older versions of ghc and cabal-install from the official EPEL repo, just run the install command:

`sudo yum install ghc cabal-install`

For newer versions of ghc you can use the unofficial Fedora Copr repos:

*   [petersen/ghc-8.6.5 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.6.5/)
*   [petersen/ghc-8.4.4 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.4.4/)
*   [petersen/ghc-8.2.2 Copr repo (EL7,EL6)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.2.2/)  
*   [petersen/ghc-8.0.2 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.0.2)
*   [petersen/ghc-7.10.3 Copr repo (EL7,EL6)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.10.3)
*   [petersen/ghc-7.8.4 Copr repo (EL7,EL6)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.8.4)
*   [petersen/ghc-7.4.2 Copr repo (EL6,EL5)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.4.2)

Note that the different ghc package versions cannot be installed in parallel.

Steps to setup stack are [on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#fedora). You can also obtain stack from the fedora [petersen/stack Copr repo](https://copr.fedoraproject.org/coprs/petersen/stack/)

### Arch Linux

The official repos on Arch Linux contain packages `ghc`, `cabal-install`, `happy`, `alex`, `haddock`. Install them with:

<pre>sudo pacman -S ghc cabal-install happy alex haskell-haddock-library</pre>

Steps to setup stack are [on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#arch-linux).

### openSUSE Leap

*   Leap 15.1 has ghc-8.6.4 and cabal-install-2.4.0.0

To install from official openSUSE:Leap repo, just run the install command:

`sudo zypper in ghc cabal-install`

For last stable version you can use the development openSUSE repository:

* [devel:languages:haskell repo](https://build.opensuse.org/project/show/devel:languages:haskell)

Use this command to add repository to your system:

`sudo zypper ar -f -p 90 https://download.opensuse.org/repositories/devel:/languages:/haskell/openSUSE_Leap_15.1/devel:languages:haskell.repo`


### openSUSE Tumbleweed

*   Tumbleweed has last stable version of ghc and cabal install.

To install from official openSUSE:Tumbleweed repo, just run the install command:

`sudo zypper in ghc cabal-install`


### Generic Tarballs

Generic minimal installers that work on most modern linux distributions are available via the [Haskell Platform](https://www.haskell.org/platform/linux.html#linux-generic)

## Manual install

To install GHC and Cabal manually, follow these steps.

### 1. Install GHC

GHC has its own web site with license information, FAQ, download links and changelogs. Depending on your operating system, there should be a package made for its package manager, otherwise (e.g. Windows) it will be an installer.

You can also download the .tar.gz/.zip and unpack and install the executables and so forth manually.

Or you can even install from source, for which [there is documentation](https://ghc.haskell.org/trac/ghc/wiki/Building).

[Download GHC now →](https://www.haskell.org/ghc/download.html)

### 2. Install Cabal-install

After installing GHC, you will want the Haskell package manager:

[Get the Cabal archive →](http://hackage.haskell.org/package/cabal-install)

Download the tar.gz file, extract and inside the resulting directory, run:

```bash
$$ sh ./bootstrap.sh
```

This will automatically download and install all the packages necessary to setup Cabal install.

Once complete, you should add `$$HOME/.cabal/bin` to your PATH. A simple way to do this is to edit your `~/.bashrc` and place in there:

```bash
export PATH=$$HOME/.cabal/bin:$$PATH
```

Now you should be able to run cabal:

```bash
$$ cabal --version
cabal-install version 1.18.0.2
using version 1.18.1.2 of the Cabal library
```

You can now update your package set:

```bash
$$ cabal update
```

And install proced to use cabal with your projects.

### 3. Installing Stack

Generic stack binary downloads are available from the [stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#linux).
