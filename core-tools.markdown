---
title: Learn about the core tools
page: core-tools
isDocumentation: true
---

# Learn about the core tools

In this guide we'll take a look at a few core tools that are installed
with the Haskell toolchain, namely, `ghc`, `runghc` and `ghci`.
These tools can be used to compile, interpret or explore Haskell programs.

Note: if you installed your Haskell toolchain using the stack tool only
and these programs are not available, prefix the commands with
`stack exec -- `, So `<command>` becomes `stack exec -- <command>`.
See the documentation in the
[Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/#exec)
for more details.

- [Compiling programs with ghc](#compiling-programs-with-ghc)
    - [Common options for GHC](#common-options-for-ghc)
        - [Warnings](#warnings)
        - [Optimisations](#optimisations)
- [An interactive environment](#an-interactive-environment)
    - [Using external packages in ghci](#using-external-packages-in-ghci)
- [Additional information](#additional-information)

First, let's start by opening your system's command line interface
and running `ghc --version` to make sure we have successfully
installed a Haskell toolchain:

```
➜ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.0.1
```

If this fails, consult [the downloads page](/downloads) for information on
how to install Haskell on your computer.

## Compiling programs with ghc

Running `ghc` invokes the Glasgow Haskell Compiler (GHC), and can be used to
compile Haskell modules and programs into native executables and libraries.

Create a new Haskell source file named `hello.hs`,
and write the following code in it:

```hs
main = putStrLn "Hello, Haskell!"
```

Now, we can compile the program by invoking `ghc` with the file name:

```sh
➜ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

GHC created the following files:

1. `hello.hi` - Haskell interface file, we won't discuss that in this article
2. `hello.o` - Object file, the output of the compiler before linking,
   we won't discuss that in this article either.
3. `hello` (or `hello.exe` on Microsoft Windows) - A native runnable
   executable.

GHC will produce an executable when the source file satisfies both conditions:

1. Defines the `main` function in the source file
2. Defines the module name to be `Main` (this can be done by adding
   `module Main where` at the top of the file), or does not have
   a module declaration (which is then inferred as the module `Main`).

Otherwise, it will only produce the `.o` and `.hi` files.

In our case, we have defined `main` and omitted the module declaration,
so GHC created an executable for us. And we can run it:

```sh
➜ ./hello 
Hello, Haskell!
```

Alternatively, we can skip the compilation phase by using the command `runghc`:

```sh
➜ runghc hello.hs
Hello, Haskell!
```

`runghc` interprets the source file instead of compiling it and does not
create build artifacts. This makes it very useful when developing programs
and can help accelerate the feedback loop. More information about `runghc`
can be found in the
[GHC user guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/runghc.html).

### Common options for GHC

Here are a few notable options we can use with `ghc` and `runghc`.
Since we already successfully compiled our `hello.hs` program and
produced an executable, We'll have to use the flag `-fforce-recomp`
to force recompilation of our `hello.hs` source file.
Otherwise GHC will notice that the content of `hello.hs` hasn't changed
and will skip the recompilation.

#### Warnings

The `-Wall` flag will enable GHC to emit many warnings about our code
(but not all warnings available, contrary to its name).
I strongly recommend always using it.

Let's compile our `hello.hs` program again, this time with `-Wall`:

```sh
➜ ghc -Wall hello.hs -fforce-recomp
[1 of 1] Compiling Main             ( hello.hs, hello.o )

hello.hs:1:1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature: main :: IO ()
  |
1 | main = putStrLn "Hello, Haskell!"
  | ^^^^
Linking hello ...
```

GHC has successfully compiled our program, but it has also emitted a warning about
not annotating `main` with a type signature. While Haskell can infer
the types of most expressions, it is recommended that top-level definitions
are annotated with their types.

We can remedy that by adding the following line above our main definition:

```hs
main :: IO ()
```

Now our `hello.hs` source file looks like this:

```hs
main :: IO ()
main = putStrLn "Hello, world!"
```

And now GHC will compile `hello.hs` without warnings.

`-Wall` emits many useful warnings that can easily be bugs,
including warnings about name shadowing, unused variables, and more.

Note that `-Wall` does not stop GHC from compiling your program.
If this is something you'd like to do, specifying the flag `-Werror`
will convert warnings to errors and will halt compilation when a warning
is emitted.

#### Optimisations

Another very useful flag is `-O`. This flag asks GHC to compile the program
with optimisations and code improvements at the cost of taking longer to compile.
This will often make the program run much faster and perform more aggressive
optimisations such as inlining and specialisation.
For even more optimisations that may take significantly longer to compile,
`-O2` is also available.

## An interactive environment

GHC provides an interactive environment in a form of a
Read-Evaluate-Print Loop (REPL) called GHCi.
To enter the environment run the program `ghci`.

```sh
➜ ghci
GHCi, version 9.0.1: https://www.haskell.org/ghc/  :? for help
ghci> 
```

It provides an interactive prompt where Haskell expressions can be written and
evaluated.

For example:

```sh
ghci> 1 + 1
2
ghci> putStrLn "Hello, world!"
Hello, world!
```

We can define new names:

```sh
ghci> double x = x + x
ghci> double 2
4
```

We can write multi-line code by surrounding it with `:{` and `:}`:

```hs
ghci> :{
| map f list =
|     case list of
|         [] -> []
|         x : xs -> f x : map f xs
| :}
ghci> map (+1) [1, 2, 3]
[2,3,4]

```

We can import Haskell source files using the `:load` command (`:l` for short):

```sh
ghci> :load hello.hs
[1 of 1] Compiling Main             ( hello.hs, interpreted )
Ok, one module loaded.
ghci> main
Hello, Haskell!
```

As well as import library modules:

```sh
ghci> import Data.Bits
ghci> shiftL 32 1
64
ghci> clearBit 33 0
32
```

We can even ask what the type of an expression is using the `:type` command
(`:t` for short):

```sh
λ> :type putStrLn
putStrLn :: String -> IO ()
```

To exit `ghci`, use the `:quit` command (or `:q` for short)

```sh
ghci> :quit
Leaving GHCi.
```

A more thorough introduction to GHCi can be found in the
[GHC user guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/ghci.html).

### Using external packages in ghci

By default, GHCi can only load and use packages that are
[included with the GHC installation](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/9.0.1-notes.html#included-libraries).

However, users of [cabal-install](https://www.haskell.org/cabal) and
[stack](http://haskellstack.org) can download and load external packages
very easily using the following commands:

cabal-install:

```sh
cabal repl --build-depends async --build-depends say
```

Stack:

```sh
stack exec --package async --package say -- ghci
```

And the modules of the relevant packages will be available for import:

```sh
GHCi, version 9.0.1: https://www.haskell.org/ghc/  :? for help
ghci> import Control.Concurrent.Async 
ghci> import Say
ghci> concurrently_ (sayString "Hello") (sayString "World")
Hello
World
```

Stack users can also use this feature with `runghc` and `ghc` by replacing
`ghci` in the command above with the wanted command.

Many more packages are waiting for you on [Hackage](https://hackage.haskell.org).

## Additional information

This article covered the most common usage of the core tools GHC offers.
If you'd like to learn more about them,
[the GHC manual](https://downloads.haskell.org/ghc/latest/docs/html/users_guide)
contains additional information on how to use the tools, including how to control
the runtime system and how to profile Haskell programs.
