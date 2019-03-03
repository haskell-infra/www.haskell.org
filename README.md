# Haskell.org hakyll

Haskell.org as a nix derivation of a hakyll built static site. The `default.nix` file returns a set with two elements
- builder (the hakyll binary which processes source into the static site)
- built (the static site built by the builder, and ready to serve)

### Developing

Simply run `nix-shell`. This will allow you to build the `site` binary which in turn builds the static site.
You may also edit the content of the site in the shell.

### Editing

You may install the site binary locally with `nix-env -f . -iA builder`. Once `site` is on your path you can edit content, and have
the site served with `site watch`.

### Building

To obtain the static site simply run `nix-build -A built` and the generated `result` link will contain the static site contents.
