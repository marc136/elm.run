# Ulm WASM REPL

## Technical Behavior

ulm-repl.ts initializes the wasm compiler and downloads the core and json packages.

The `ReplInput` custom element renders codemirror and emits the `compiler` events `loading`, `ready`, `error: Error`.\
Elm will maybe get a button to trigger compilation, in this case I will either create a port msg or set a property (attribute would need revoking).\
But for now, the custom element will inform when Elm needs to store/show something.

## DONE

* Clear input when evaluating to non-error
* If the user reuses the same name, the old history entry is marked as outdated (declarations and type definitions)


## TODO

Next step: Styling
1. Take from the try-editor only the styles that I need.
2. Create one css file.
3. Use same colors for errors and for codemirror
4. Switching theme should change whole page, not just codemirror

### Further steps

Add button to copy input from history into input field

Style declarations that were overwritten differently in logs

Fake elm-format

Prepend missing type declarations

Allow user to manually remove items from log

Allow user to auto-clean all outdated entries from log

Beginner/Zen mode: Hide type definitions?

Render all entries as one Elm file
    Allow download of that Elm file

?Clear input after every evaluate and add [try again] button to copy from last failure
