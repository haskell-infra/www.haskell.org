# Haskell.org Website

This is the website for www.haskell.org built as a hakyll static site, which builds both as a nix derivation and a standalone cabal project. Issues with the site can be raised in this repository, and PRs can be made to change content. More general administrative issues with the site or related haskell.org infrastructure are better raised directly with the admin team on the #haskell-infrastructure channel on freenode, or at the admin@[LANGUAGE].org email address.

### Subsites

Not all subsites of www.haskell.org are built from this repository.
Some of the others are

* www.haskell.org/cabal (built from [cabal-website](https://github.com/haskell/cabal-website))

### Cabal instructions
Just run `cabal v2-build` to build or `cabal v2-run` to run, and `cabal v2-run -- build` to actually build the site.

### Nix instructions

This repo provides haskell.org as a nix derivation of a hakyll built static site. The `default.nix` file returns a set with two elements
- builder (the hakyll binary which processes source into the static site)
- built (the static site built by the builder, and ready to serve)

### Developing

Simply run `nix-shell`. This will allow you to build the `haskell-org-site` binary which in turn builds the static site.
You may also edit the content of the site in the shell.

### Editing

You may install the `haskell-org-site` binary locally with `nix-env -f . -iA builder`. Once `haskell-org-site` is on your path you can edit content, and have
the site served with `site watch`.

### Building

To obtain the static `haskell-org-site` simply run `nix-build -A built` and the generated `result` link will contain the static site contents.
