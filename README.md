# Haskell.org Website

This repository holds the source code and content for
[www.haskell.org](http://www.haskell.org).  Questions about and issues with the site can
be raised in this repository, and PRs can be made to change
content. More general administrative issues with the site or related
haskell.org infrastructure are better [raised directly with the admin
team](https://github.com/haskell-infra/haskell-admins#the-team-and-how-to-contact-them).
Not everything beneath `www.haskell.org` is generated from this
repository.  See [the list of other subsites](#subsites) below.

## Developing the website

The website is built as a Hakyll static site, which builds both as a
nix derivation and a standalone cabal project.

### Community contributions

We welcome contributions from the community.  To see which things we
need particular help with, please see the [community contributions
meta-ticket](https://github.com/haskell-infra/www.haskell.org/issues/177).

If you have your own idea for a change you would like to see you are
welcome to [submit a
PR](https://github.com/haskell-infra/www.haskell.org/pulls). Before
you start work on your idea you might like to [file a new
issue](https://github.com/haskell-infra/www.haskell.org/issues/new) to
discuss it with the maintainers to get an estimate for how likely it
is to be accepted.  PRs are accepted according to [the PR policy of
this
repository](https://github.com/haskell-org/committee/blob/main/proposals/0003-pr-process.md).

### Making Changes

The easiest way to see the effect of your changes is to use the `buildAndWatch` script and then
point your web browser to [http://localhost:8000](http://localhost:8000). When
you are finished editing or want to re-build the Hakyll part of the site, you
can stop the script by pressing `Control+c` (`C-c`).

If you are only making changes to the content of the site, you can leave this
script running and it will automatically pick up changes and re-build the site
for you.

If you want to change the `builder`, or if you encounter an error where your
changes to the content aren't being picked up, need to stop the script and
re-start it.

### Submitting Changes

Once you're satisfied with your changes, make a PR and the maintainers will try
to review it as soon as we can.

### Working On The Builder

The `builder` is the static site generator that turns the markdown files, CSS,
images, and scripts into a website. It lives in the `builder`. Most of the time,
you won't need to make changes to the builder and you can follow the
instructions in the _Contributing Changes_ section above.

If you want to make a quick change or two, you can continue to use the
`buildAndWatch` script to rebuild changes, but for more substantial changes this
might increase the build cycle time too much. In this case, you can build the
builder using either nix or cabal. Directions for both are given below:

<a id="buildingWithoutNix"></a>
### Manually Building the Site With Cabal

If you don't have nix installed, or if you are inside of the project's nix
shell, you will want to use cabal to compile the builder. To do so, enter the
`builder` directory and compile the program with:

```shell
cabal v2-build
```

Once compiled, the builder must be run from the project root directory so that
it can find all of the content. To run the builder, you need to first find the
name of the executable. From the builder directory, you can find the executable
path by running:

```
cabal v2-exec -- which haskell-org-site
```

Using that path, you can run the builder from the project root directory.

### Manually Building the Site With Nix

If you have nix installed, you can have nix build the builder by running:

```
nix-build -A builder
```

You may then run the builder binary from the `result` directory:

```
./result/bin/haskell-org-site build
```

### Deploying

The site will automatically be deployed live to <http://www.haskell.org/> every time a branch is merged to `master`. Alternatively an admin for this GitHub repository can deploy the site by visiting the [Deploy workflow page](https://github.com/haskell-infra/www.haskell.org/actions/workflows/deploy.yml), clicking the "Run workflow" dropdown, choosing the branch to build and deploy, and clicking the "Run workflow" button.

<a id="subsites"></a>
# Subsites

Not all subsites of www.haskell.org are built from this repository.
Some of the others are

| Subsite | Source | Owner | Details |
| --------------- | --------------- | --------------- | --------------- |
| [`cabal/`](https://www.haskell.org/cabal/) | [cabal-website](https://github.com/haskell/cabal-website) | Cabal maintainers | [Details](#details-cabal) |
| [`ghc/`](https://www.haskell.org/ghc/) | |  | [Details](#details-ghc) |
| [`ghc-perf/`](https://www.haskell.org/ghc-perf/) | | Joachim Breitner | [Details](#details-ghc-perf) |
| [`ghcup/`](https://www.haskell.org/ghcup/) | [ghcup-hs](https://gitlab.haskell.org/haskell/ghcup-hs/-/tree/master/www) | Julian Ospald | |
| [`haddock/`](https://www.haskell.org/haddock/) | Redirect | Haddock maintainers | [Details](#details-haddock) |
| [`haskell-symposium/`](https://www.haskell.org/haskell-symposium/) | | Haskell Symposium organisers | |
| [`platform/`](https://www.haskell.org/platform/) | Redirect | | |
| [`alex/`](https://www.haskell.org/alex/) | | Alex maintainers | [Details](#details-alex) |
| [`arrows/`](https://www.haskell.org/arrows/) | | Ross Patterson | |
| [`communities/`](https://www.haskell.org/communities/) | | | [Details](#details-communities) |
| [`definition/`](https://www.haskell.org/definition/) | | Haskell Prime committee | [Details](#details-definition) |
| [`happy/`](https://www.haskell.org/happy/) | | Happy maintainers | [Details](#details-happy) |
| [`haskell-workshop/`](https://www.haskell.org/haskell-workshop/) | | Haskell Symposium organisers | [Details](#details-haskell-workshop) |
| [`hugs/`](https://www.haskell.org/hugs/) | | | [Details](#details-hugs) |
| [`nhc98/`](https://www.haskell.org/nhc98/) | | York Functional Programming Group | [Details](#details-nhc98) |
| [`onlinereport/`](https://www.haskell.org/nhc98/) | | Haskell Prime committee | [Details](#details-onlinereport) |
| [`tutorial/`](https://www.haskell.org/tutorial/) | | | [Details](#details-tutorial) |

## Details on subsites

* <a name="details-cabal"></a>
  `cabal/`

  At least @emilypi has deploy permissions.

* <a name="details-ghc"></a>
  `ghc/`

  Probably owned by the GHC team.  Most likely @bgamari is the best
  contact point.

* <a name="details-ghc-perf"></a>
  `ghc-perf/`

  Same as https://perf.haskell.org

* <a name="details-alex"></a>
  `alex/`

* <a name="details-communities"></a>
  `communities/`

  Just contains a link to the HCAR page on this Haskell wiki

* <a name="details-definition"></a>
  `definition/`

  Last update: 21 June 2005.  [Should be owned
  by](https://github.com/haskell-infra/www.haskell.org/pull/103#issuecomment-877643716)
  the same owners as [`onlinereport`](#details-onlinereport).

* <a name="details-haddock"></a>
  `haddock/`

* <a name="details-happy"></a>
  `happy/`

* <a name="details-haskell-workshop"></a>
  `haskell-workshop/`

  Archival page.  Not updated since 2008.  Source is linked as
  http://abridgegame.org/darcs/ but that link seems long-dead.
  [Ultimately the responsibility of the Haskell Symposium
  organisers](https://github.com/haskell-infra/www.haskell.org/pull/103#issuecomment-877643716).

* <a name="details-hugs"></a>
  `hugs/`

  Archival page.  Not updated since 2003. Report problems to
  <hugs-bugs@haskell.org>.  Mark Jones and Malcolm Wallace [suggested
  as](https://github.com/haskell-infra/www.haskell.org/pull/103#issuecomment-877643716)
  the points of contact.

* <a name="details-nhc98"></a>
  `nhc98/`

  Archival page.  This page last modified: 9th July 2010.  Not updated
  since 2010.  Mark Jones and Malcolm Wallace [suggested
  as](https://github.com/haskell-infra/www.haskell.org/pull/103#issuecomment-877643716)
  the points of contact.

* <a name="details-onlinereport"></a>
  `onlinereport/`

* <a name="details-tutorial"></a>
  `tutorial/`

  Revised June 2000. "This code has been tested with Hugs
  98". Copyright (C) 1999 Paul Hudak, John Peterson and Joseph Fasel.
  Perhaps confer with Paul Hudak's former students and collaborators.
