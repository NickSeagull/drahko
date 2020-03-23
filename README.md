# 🐉⌨ Idris AutoHotkey

[![Build status](https://ci.appveyor.com/api/projects/status/b5r11wcau0upomgv?svg=true)](https://ci.appveyor.com/project/NickSeagull/idris-autohotkey)

> ⚠ **DISCLAIMER:** This project is in it's very early days, and it doesn't support most of stuff from Idris or AutoHotkey. The only tested code is the one in [`examples/HelloWorld.idr`](examples/HelloWorld.idr), which is a Hello world app.

![idris autohotkey screenshot](https://pbs.twimg.com/media/ESnLF6SWsAIhzRF?format=png&name=small)

This project attempts to improve the life of AutoHotkey developers, given that the language has nearly no
development-time errors, and the runtime ones are silent, making it very difficult to work with.

By generating the code automatically, we free ourselves from AutoHotkey's silent syntax errors, like incorrect calls to subroutines.

The Idris typechecker should remove the rest of runtime errors.

## 🏗 Building

1. Follow the steps on how to install Idris dependencies using Stack [on the official Idris wiki](https://github.com/idris-lang/Idris-dev/wiki/Idris-on-Windows#stack-haskell-platform-tool-installation).
2. Clone the repo
   ```text
   git clone git@github.com:NickSeagull/idris-autohotkey.git
   cd idris-autohotkey
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

This project is largely based from [owickstrom](https://github.com/owickstrom)'s [`idris-vimscript`](https://github.com/owickstrom/idris-vimscript), licensed under BSD3 license. Thanks for keeping the repo even though it is not maintained, as it was the key for starting this project.
