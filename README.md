# Idris AutoHotkey 🐉⌨

> ⚠ **DISCLAIMER:** This project is in it's very early days, and it doesn't support most of stuff from Idris or AutoHotkey. The only tested code is the one in [`examples/simple.idr`](examples/simple.idr), which is a Hello world app.

![idris autohotkey screenshot](https://pbs.twimg.com/media/ESnLF6SWsAIhzRF?format=png&name=small)

This project attempts to improve the life of AutoHotkey developers, given that the language has nearly no
development-time errors, and the runtime ones are silent, making it very difficult to work with.

By generating the code automatically, we free ourselves from AutoHotkey's silent syntax errors, like incorrect calls to subroutines.

The Idris typechecker should remove the rest of runtime errors.

## Acknowledgements

This project is largely based from [owickstrom](https://github.com/owickstrom)'s [`idris-vimscript`](https://github.com/owickstrom/idris-vimscript), licensed under BSD3 license. Thanks for keeping the repo even though it is not maintained, as it was the key for starting this project.
