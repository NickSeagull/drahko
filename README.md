# This project is unmaintained:

> The project was an attempt to take advantage of the AutoHotkey libraries while having the power of Idris. After many tests, AutoHotkey is slower than I initially thought, so some code that is generated ends up being very slow. Might be useful for quick hotkeys, but is not useful when having sticky-key type hotkeys (hotkeys that are `letter + letter` rather than `modfiier + letter`) as the keyboard typing becomes very slow.

<h1 style="text-align:center"><img src="./assets/logo.png" /></h1>

> Boost your efficiency: Make your computer work for you.

[![Build status](https://ci.appveyor.com/api/projects/status/b5r11wcau0upomgv?svg=true)](https://ci.appveyor.com/project/NickSeagull/drahko)
[![Conventional Commits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-yellow.svg)](https://conventionalcommits.org)
[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)
[![Mergify Status][mergify-status]][mergify]
[![Hintman](https://img.shields.io/badge/%F0%9F%94%ABhintman-enabled-blueviolet)](https://github.com/kowainik/hintman)

Drahko is a Windows automation framework based on [AutoHotkey](autohotkey.com),
a widely used and stable automation solution.

It checks your code for possible errors during compilation, so they don't
appear while running your scripts.

Drahko leverages the [Idris](https://www.idris-lang.org/) compiler, in order
to generate highly stable and correct AutoHotkey code. **Don't go reading
the Idris documentation now, everything you need is in the [Drahko
documentation](https://github.com/NickSeagull/drahko/issues/11)**.

Writing a _Hello World_ application with Drahko goes like this:

```idris
import Drahko

main : Promise ()
main = msgBox "Hello world!"
```

Want to know more? Head over to [the documentation site](https://github.com/NickSeagull/drahko/issues/11)

## ⌨ Improvements over AutoHotkey

One of the goals of this project is to improve the life of AutoHotkey developers,
given that AutoHotkey has nearly no development-time errors, and the runtime ones
are silent, making it very difficult to work with.

By generating the code automatically, we free ourselves from AutoHotkey's silent
syntax errors, like incorrect calls to subroutines or commands. The Idris
typechecker should remove the rest of runtime errors.

In addition to that, Drahko:

* Allows interacting easily with the terminal, which is something basic in any
  programming language.
* Has a dependency manager to install libraries from other users of Drahko
* Provides a consistent syntax, so you don't have to write `MsgBox, % Array%i%[i]`
  anymore, or wonder about the difference between `:=` and `=`.

## 🏗 Building

1. Follow the steps on how to install Idris dependencies using Stack
   [on the official Idris wiki](https://github.com/idris-lang/Idris-dev/wiki/Idris-on-Windows#stack-haskell-platform-tool-installation).
2. Clone the repo

   ```text
   git clone git@github.com:NickSeagull/drahko.git
   cd drahko
   ```

3. Build the project

   ```text
   stack build
   ```

4. Try to compile the example

   ```text
   stack exec -- idris -i lib .\examples\HelloWorld.idr --codegen autohotkey -o .\examples\HelloWorld.ahk
   ```

## 🙏 Acknowledgements

This project is largely based from [owickstrom](https://github.com/owickstrom)'s
[`idris-vimscript`](https://github.com/owickstrom/idris-vimscript), licensed
under BSD3 license. Thanks for keeping the repo even though it is not maintained,
as it was the key for starting this project.

[mergify]: https://mergify.io
[mergify-status]: https://img.shields.io/endpoint.svg?url=https://gh.mergify.io/badges/NickSeagull/drahko&style=flat
