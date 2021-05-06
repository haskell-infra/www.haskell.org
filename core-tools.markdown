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
See the documentation in the [Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/#exec) for more details.

- [Compiling programs with ghc](#compiling-programs-with-ghc)
    - [Common options for GHC](#common-options-for-ghc)
        - [Warnings](#warnings)
        - [Optimisations](#optimisations)
- [An interactive environment](#an-interactive-environment)
    - [Using external packages in ghci](#using-external-packages-in-ghci)
- [Compiling programs - Advanced usage](#compiling-programs---advanced-usage)
    - [Runtime options](#runtime-options)
    - [Profiling](#profiling)

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

Running `ghc` invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell
modules and programs into native executables and libraries.

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
2. `hello.o` - Object file, the output of the compiler before linking, we won't discuss that in this article either.
3. `hello` (or `hello.exe` on Microsoft Windows) - A native runnable executable.

GHC will produce an executable when the source file:

1. Defines the binding `main` in the source file
2. Defines the module name to be `Main` (this can be done by adding `module Main where` at the top of the file), or does not have a module declaration (which is then inferred as the module `Main`).

Otherwise, it will only produce the `.o` and `.hi` files.

In our case, we have defined `main` and omitted the module declaration, so GHC created an executable for us. And we can run it:

```sh
➜ ./hello 
Hello, Haskell!
```

Or on Microsoft Windows:

```sh
> hello.exe
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
can be found in the [GHC user guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/runghc.html).

### Common options for GHC

Here are a few notable options we can use with `ghc` and `runghc`.
Make sure to delete the `hello.o`, `hello.hi` and `hello` files before
attempting to recompile `hello.hs` - GHC will not recompile it
if the compilation output for the same file already exists.

#### Warnings

The `-Wall` flag will enable GHC to emit many warnings about our code (but not all warnings available, contrary to its name).
I strongly recommend always using it.

Let's compile our `hello.hs` program again, this time with `-Wall`:

```sh
➜ ghc -Wall hello.hs
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

And now (after deleting the build artifact files) GHC will compile `hello.hs`
without warnings.

`-Wall` emits many useful warnings that can easily be bugs,
including warnings about name shadowing, unused variables, and more.

Note that `-Wall` does not stop GHC from compiling your program.
If this is something you'd like to do, specifying the flag `-Werror`
will convert warnings to errors and will halt compilation when a warning is emitted.

#### Optimisations

Another very useful flag is `-O`. This flag asks GHC to compile the program with optimisations and code improvements at the cost of taking longer to compile.
This will often make the program run much faster and perform more aggressive optimisations such as inlining and specialisation.
For even more optimisations that may take significantly longer, `-O2` is also available.

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

We can import Haskell source files using the `:load` command:

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

We can even ask what the type of an expression is using the `:type` command:

```sh
λ> :type putStrLn
putStrLn :: String -> IO ()
```

To exit `ghci`, use the `:quit` command (or `:q` for short)

```sh
ghci> :quit
Leaving GHCi.
```

A more thorough introduction to GHCi can be found in the [GHC user guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/ghci.html).

### Using external packages in ghci

By default, ghci can only load and use packages that are [included with the GHC installation](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/9.0.1-notes.html#included-libraries).

However, users of [cabal-install](https://www.haskell.org/cabal) and [stack](http://haskellstack.org) can download and load external packages very easily using the following commands:

Cabal-install:

```sh
cabal repl --build-depends random
```

Stack:

```sh
stack exec --package random -- ghci
```

And the modules of the relevant packages will be available for import:

```sh
GHCi, version 9.0.1: https://www.haskell.org/ghc/  :? for help
λ> import System.Random
λ> randomRIO (1, 10)
1
λ> randomRIO (1, 10)
8
λ> randomRIO (1, 10)
5
```

Stack users can also use this feature with `runghc` and `ghc` by replacing `ghci` in the command above.

Many more packages are waiting for you on [Hackage](https://hackage.haskell.org).

## Compiling programs - Advanced usage

In this section we'll cover more advanced usage available when compiling programs.
Feel free to skip it!

### Runtime options

Compiling a Haskell program with the flag `-rtsopts` provides us with
the ability to more finely control the Haskell runtime system when running the program.

After compiling with `-rtsopts` with can pass flags to the Haskell runtime by
listing them between `+RTS` and `-RTS` when invoking our Haskell program.
For example, let's compile our `hello.hs` program with runtime options:

```hs
➜ ghc -rtsopts hello.hs 
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

And run it with `-sstderr` which will print some information
about the runtime execution of the program: 

```sh
➜  ./hello +RTS -sstderr -RTS
Hello, Haskell!
          52,064 bytes allocated in the heap
           3,312 bytes copied during GC
          44,408 bytes maximum residency (1 sample(s))
          25,224 bytes maximum slop
               2 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.000s  (  0.000s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.000s  (  0.000s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    717,569,877 bytes per MUT second

  Productivity  27.4% of total user, 28.0% of total elapsed
```

This information is not very useful for such a trivial program,
but as programs get more complex it will provide more insight
regarding the runtime of the program.

Alternatively, it is also possible to set the exact runtime flags
that should be used when compiling the program using `-with-rtsopts='<flags>'`.

For example:

```sh
➜ ghc -with-rtsopts='-sstderr' hello.hs 
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

And then run the executable without arguments:

```sh
➜ ./hello 
Hello, Haskell!
          52,064 bytes allocated in the heap
           3,312 bytes copied during GC
          44,408 bytes maximum residency (1 sample(s))
          25,224 bytes maximum slop
               2 MiB total memory in use (0 MB lost due to fragmentation)

...
```

The full list of runtime options can be found in the [GHC user guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/runtime_control.html).

### Profiling

The GHC user guide contains a very thorough [section about profiling Haskell programs](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html).

