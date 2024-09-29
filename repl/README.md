# REPL for https://elm.run/repl

Page where the user may enter and evaluate Elm code.\
The user may enter multiple lines, but only one Elm expression at a time.\
On each keystroke, (inferred) types and the evaluated result are shown.

It is also usable on mobile without extensive zooming involved.

## What it is and why I built it (too long, won't read)

### What it is

This REPL has an input field with syntax highlighting where the user may insert
multiple lines, use the cursor keys to go up or down and change the input
however she wants.\
The input field grows or shrinks as needed.

On every keystroke, the code is type-checked and evaluated. Type information is
shown above the entered text, and the evaluated result is shown below.

When pressing [Ctrl]+[Enter] or the "Run code" button, the full compiler result
is added to the history above the input area.\
Compiler problems are pretty-printed.

From the history, the user may "edit" to copy the code into the input aread
below and alter it as she sees fit.\
She can also delete individual entries, or she can use the "Clean up" button to
remove multiple entries:\
For instance remove all compiler errors, or all evaluated expressions, or remove
everthing except the valid declarations and definitions.

On my phone, I don't need to scroll around on the page to enter or evaluate
code. Horizontal overflow of the individual fixed-width formatted Elm compiler
errors is scrollable.

### Why I built it

My main gripe with most programming REPLs is that they only allow to enter one
line at a time. And that changing what I entered two lines above is usually a
pain or often not directly possible.

When I'm on my phone, I had no good way of writing Elm code.

Alternatives that I know of always require a connection to a server to evaluate
the code.

## DEV

```sh
# keep building the compiler
cd ../ulm-compiler-wasm
./init.sh
./dev.sh

# And start the esbuild server and elm-watch
npm run dev
```

## Changelog

### Initial public release

- Reuse code from https://elm-lang.org/try to render compiler problems
- Add light/dark/auto mode for entire page
- Allow user to manually remove items from log
- Allow user to auto-clean all outdated entries from log
- Clear input when evaluating to non-error
- Add "edit" button as a shorthand to copy code from history to REPL input field
- If the user reuses the same name, the old history entry is marked as outdated
  (declarations and type definitions)
- Can insert 4 different examples with multiple evaluations for demo purposes

## TODO

### Further steps

- Fake elm-format
- Prepend missing type declarations
- Beginner/Zen mode: Hide type definitions?
- Render all entries as one Elm file
- Allow download of that Elm file
- Turn into a PWA (progressive web app) so it can be installed to computers/phones and used offline.
