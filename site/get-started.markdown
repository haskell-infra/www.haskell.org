---
title: Get Started
page: get-started
isGettingStarted: true
---

# Get started

Welcome, new Haskeller! Read on to run your first lines of code, set up your Haskell development environment, and get directions for further learning.

## Content

  - [Running your first lines of code](#running-your-first-lines-of-code)
  - [Set up a Haskell development environment](#set-up-a-haskell-development-environment)
    - [GHCup: universal installer](#ghcup-universal-installer)
    - [Editor](#editor)
  - [Writing your first Haskell program](#writing-your-first-haskell-program)
  - [Participate in the community](#participate-in-the-community)
  - [Next steps](#next-steps)

## Running your first lines of code

The Haskell compiler is called GHC. It comes with an interactive interpreter called GHCi which is great for playing with Haskell and trying things out.

The simplest way to give it a spin is to [install Nix](https://nix.dev/install-nix), which allows running `ghci` without installing it permanently:

```shell-session
nix-shell -p ghc --run ghci
```

Let's do a simple calculation to check Haskell's computing capabilities:

```
> 6 + 3^2 * 4
42
```

42 is a nice even number, but what about the first 10 even numbers after it?
```
> take 10 (filter even [43..])
[44,46,48,50,52,54,56,58,60,62]
```

What is the sum of those?
```
> sum it
530
```
**NOTE**: We used a special feature of GHCi here, which is a special `it` variable that remembers the result of the last expression.

Great, you got the first taste of Haskell! Now let's get to running a real program.

## Set up a Haskell development environment

A complete Haskell development environment consists of the Haskell toolchain (compiler, language server, build tool) and an editor with good Haskell support. The quickest way to get this set up is to:

1. Use **GHCup** to install and manage the Haskell toolchain.
2. Use [**VSCode**](https://code.visualstudio.com/) as the editor, with the Haskell extension installed.

### GHCup: universal installer

[GHCup](https://www.haskell.org/ghcup/#) is a universal installer for Haskell that will install everything you need to program in Haskell, and will help you manage those installations (update, switch versions, ...). GHCup does not require root/admin access. Follow the instructions on the [GHCup webpage](https://www.haskell.org/ghcup/#) to install GHCup. Then use it to install the Haskell toolchain, which consists of:

1. **GHC** (The Haskell compiler) We will use GHC below to run our examples, but in practice you will mostly use a tool like Cabal or Stack to build your code, instead of GHC directly.
2. **HLS** (The Haskell Language Server) You won't use HLS directly, instead your code editor will use it in the background to provide you with a great experience while editing Haskell code.
3. **Cabal** (A Haskell build tool) You will use Cabal to structure your Haskell projects, build them, run them, define dependencies, ... .
4. **Stack** (A Haskell build tool) An alternative to Cabal.


<div class="bs-callout bs-callout-info">
  <p>
    <h4>Cabal and Stack: which one should I install?</h4>
    We recommend installing both. Most Haskell projects can be built using Cabal, but some might require Stack. Installing both guarantees that you can use either, and while following a tutorial or book you can use whatever they recommend.
  </p>
</div>

### Editor

[**Visual Studio Code**](https://code.visualstudio.com/) ([**VSCode**](https://code.visualstudio.com/)) is a popular choice with well-supported Haskell integration. Install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) and you are all set. It should work out of the box and use your installation of HLS. To learn about support for other editors, check out [HLS docs for editor configuration](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor).

## Writing your first Haskell program

We have everything set up, let's use it!

In your editor, create a new file named `hello.hs`. Write the following in it:
```hs
main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))
```

You can now compile it with `ghc` to produce an executable called `hello` that we will then run:
```
> ghc hello.hs
> ./hello
Hello, everybody!
Please look at my favorite odd numbers: [11,13,15,17,19]
```

There you go, you just wrote a short, polite program in Haskell!

**GHCI TIP**: You can also load your file directly into `ghci`, which will enable you to play with any functions and other definitions you defined in it. So for our example, we can just load `hello.hs` with GHCi and then call the function `main` like this:
```
> ghci hello.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( hello.hs, interpreted )
Ok, one module loaded.
> main
Hello, everybody!
Please look at my favorite odd numbers: [11,13,15,17,19]
```

## Participate in the community

By participating in the Haskell community, you will be able to ask for help and learn about new developments in the Haskell ecosystem. Some of the most popular places to interact with the community are:

 - [Haskell Discourse](https://discourse.haskell.org/)
 - [Haskell subreddit](https://www.reddit.com/r/haskell/)
 - [IRC](https://www.haskell.org/irc/)

We recommend joining right now, and don't be shy to ask for help! Check [https://www.haskell.org/community](https://www.haskell.org/community) for a full list of resources relating to the Haskell community.

## Next steps

Popular free learning resources for beginners:

 - [Introductory Haskell course of the University of Pennsylvania (CIS194)](https://www.seas.upenn.edu/~cis1940/spring13/lectures.html) (course)
 - [Learn You a Haskell for Great Good!](https://learnyouahaskell.github.io) (book)
 - [Learn Haskell by building a blog generator](https://learn-haskell.blog) (book)

This is just the tip of the iceberg though: check [Documentation](https://www.haskell.org/documentation/) for a bigger list of learning resources. That is it, fellow Haskeller! Enjoy learning Haskell, do (not?) be lazy and see you in the community!
