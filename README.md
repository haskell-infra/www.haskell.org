# Haskell.org Website

This is the website for www.haskell.org built as a hakyll static site, which builds both as a nix derivation and a standalone cabal project. Issues with the site can be raised in this repository, and PRs can be made to change content. More general administrative issues with the site or related haskell.org infrastructure are better raised directly with the admin team on the #haskell-infrastructure channel on freenode, or at the admin@[LANGUAGE].org email address.

### Subsites

Not all subsites of www.haskell.org are built from this repository.
Some of the others are

* www.haskell.org/cabal (built from [cabal-website](https://github.com/haskell/cabal-website))
* www.haskell.org/platform (built from [haskell-platform](https://github.com/haskell/haskell-platform/tree/master/website))
* www.haskell.org/ghcup (build from [ghcup-hs](https://gitlab.haskell.org/haskell/ghcup-hs/-/tree/master/www)

### Nix

The best way to make changes to haskell.org is to use nix. You can install it
by following the directions [on the nix downloads page](https://nixos.org/download.html).

### Contributing Changes

The easiest way to make changes is to use the `buildAndWatch` script and then
point your web browser to [http://localhost:8000](http://localhost:8000). When
you are finished editing or want to re-build the hakyll part of the site, you
can stop the script by pressing `Control+c` (`C-c`).

If you are only making changes to the content of the site, you can leave this
script running and it will automatically pick up changes and re-build the site
for you.

If you want to change the `builder`, or if you encounter an error where your
changes to the content aren't being picked up, need to stop the script and
re-start it.

Once you're satisfied with your changes, make a PR and the maintainers will try
to review it as soon as we can.

### Working On The Builder

The `builder` is the static site generator that turns the markdown files, CSS,
images, and scripts into a website. It lives in the `builder`. Most of the time,
you won't need to make changes to the builder and you can follow the
instructions in the _Contributing Changes_ section above.

If you want to make a quick change or two, you can continue to use the
`buildAndWatch` script to rebuild changes, but for more substantial changes this
might increase the build cycle time too much. In that case, you can test your
changes more immediately by building with `cabal`.

If you want to ensure that you are using the correct versions of all of the
libraries, you can develop inside of a nix shell. To do that, simply type:

`nix-shell` from either the project root directory or the `builder` directory.

To build the builder with cabal, go into the `builder` directory and run:

```
cabal v2-build
```

### Manually Building the Site with the Builder

If you want to manually build the site with the builder, you need to run the
builder from the project root directory. You can do this by building the site
with nix:

```
nix-build -A builder
```

Then running the builder binary from the `result` directory:

```
./result/bin/haskell-org-site build
```

Alternatively you can build with cabal and run the executable directly:

```
./builder/dist-newstyle/build/x86_64-linux/ghc-8.8.4/haskell-org-0.1.0.0/x/haskell-org-site/build/haskell-org-site/haskell-org-site build
```
