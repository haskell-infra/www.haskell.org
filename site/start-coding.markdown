---
title: Start Coding now
page: start-coding
isSetUp: true
---

## Install a development environment in three steps

A Haskell development environment consists of:

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
