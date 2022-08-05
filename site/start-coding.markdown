---
title: Start Coding now
page: start-coding
isSetUp: true
---

## Set up a local development environment in three steps

A Haskell development environment consist in:

- a compiler (`ghc`)
- a language server (`hls`)
- a building tool (`cabal` and/or `stack`)
- an editor compatible with the language server protocol.

It is recommended to use `ghcup` to manage your haskell toolchain and `vscode` as your editor.

- **Step 1**: Install [GHCup](https://www.haskell.org/ghcup/). You'll be prompted to install some tools: Install `hls` and optionally `stack`. (`stack` users should [configure it properly](https://docs.haskellstack.org/en/stable/Stack_and_VS_Code/#workaround-1)).
- **Step 2**: Install [vscode](https://code.visualstudio.com/). For integration with other editors see `hls` [documentation](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor)
- **Step 3**: Open `vscode` and install the [haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) either using the extension panel or pressing CTRL+P and `ext install haskell.haskell`.

## Try without installing Haskell

If you don't want to install the Haskell toolchain locally, you have a ready-to-use environment in Gitpod.

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/gitpod-io/template-haskell)

## Integration with stack

`stack` can integrate easily with `haskell-language-server`. You need to configure `stack` so it doesn't install `ghc` on its own.

- First, _You still need to install ghc, cabal and hls via ghcup_, so read the first step in this guide. This is particulary important if you already had stack installed before.
- Configure `stack` to use system's `ghc`. Configuration can be either _global_ (recomended) or _local_
  - Global (recomended)

    ```bash
    # Run on a terminal if you want to set a global configuration
    stack config set install-ghc --global false
    stack config set system-ghc --global true 
    ```

  - Local:

    ```bash
    # On stack.yaml add this line if you want to set a local configuration
    system-ghc: true 
    ```

- You _must_ use a `stackage` snapshot compatible with the `ghc` version installed by `ghcup`. Visit [stackage](https://www.stackage.org/) to get the `ghc` version on each snapshot. For example `lts-18.28` uses `ghc-8.10.7`. You can check version compatibility with:
  
  ```bash
  # This prints the ghc's version installed and used by ghcup
  > ghcup list -c installed -t ghc
  ✔✔ ghc  8.10.7  recommended,base-4.14.3.0 hls-powered

  # Your resolver should point to 18.28      ---------------------------------------------------
  > cat stack.yaml    #                                                                    vvvvv    
  resolver:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/18/28.yaml

  ```

It is recommended to read `stack`'s user guide [on this topic](https://docs.haskellstack.org/en/stable/Stack_and_VS_Code/)

In case you need to use a different snapshot you have to install it's `ghc` version with `ghcup`. For the sake of example, let's say you are using `lts-16.31`, which uses `ghc-8.8.4`. Then you need to run `ghcup install ghc 8.8.4` on a terminal. Also, depending on which snapshot you are using, you might loose integration with `hls`. Check `hls` [documentation](https://haskell-language-server.readthedocs.io/en/latest/supported-versions.html) for more info.
