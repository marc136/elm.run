# REPL on official Elm language guide

The [official Elm language guide](https://guide.elm-lang.org) has a REPL, which
I describe a little below.

From
view-source:https://guide.elm-lang.org/gitbook/gitbook-plugin-elm-repl/repl.js

```
function init(id, node) {
    var repl = Elm.Repl.init({
        node: node,
        flags: {
            id: id,
            types: node.className.indexOf('show-types') !== -1,
            entries: JSON.parse(node.textContent)
        }
    });

    repl.ports.evaluate.subscribe(evaluate);

    function evaluate(javascript) {
        var url = URL.createObjectURL(new Blob([javascript], { mime: "application/javascript" }));
        var worker = new Worker(url);

        worker.onmessage = function(e) { report(e.data) };
        worker.onerror = function(e) { report(e.message) };

        function report(value) {
            repl.ports.outcomes.send(value);
            URL.revokeObjectURL(url);
            worker.terminate();
        }
    }
}
```

The REPL on guide.elm-lang.org is a closed-source Elm program. On every [enter]
key press, it sends the input to the server which responds either with a command
like `indent` or with js code.

For example

```
{"imports":{},"types":{"User":"type alias User = { name : String, age : Int }\n"},"decls":{},"entry":"if 13 > 12 then\n  \"12\"\n  else\n  \"horst\""}
```

returns `indent`

And a second press to [enter] will then return the js code to calculate and
print the result.

```
{"imports":{},"types":{"User":"type alias User = { name : String, age : Int }\n"},"decls":{},"entry":"if 13 > 12 then\n  \"12\"\n  else\n  \"horst\"\n  "}
```

See ../elm-compiler-wasm/worker/src/Endpoint/Repl.hs to get started

In the meantime, elmrepl.de has gotten a lot faster (it is not sponsored by
netcup.de).

---

# Behavior

When loading the page, it shows an empty input area.

Like the Elm REPL, two empty line breaks will force compilation.\
The same should happen after Ctrl+Enter.

When entering text, it will immediately try to evaluate the code.\
Input should be debounced, and it should await compilation before starting a new
one.\
If a non-error result is reached, it will be rendered below the input area
(maybe in more muted colors to indicate that it is not finished?)\
I would also like to see partially applied functions there,
`abc = List.map (\i -> i + 1)` should print

```json
{
  "name": "abc",
  "value": "\u001b[36m<function>\u001b[0m",
  "type": "List number -> List number"
}
```

And if the user continues and enters a list of numbers, it should then fully run
and display its content.

## Behavior 2

The input area is empty on start.

When entering text, it will immediately try to evaluate the code.\
Input should be debounced, and it should await compilation before starting a new
one.\
If a non-error result is reached, it will show

1. the type definition above what the user entered
2. the value below input area (maybe in more muted colors to indicate that it is
   not finished?)\
   I would also like to see partially applied functions there,
   `abc = List.map (\i -> i + 1)` should print

TODO I would like to trim empty lines above and below, but not yet sure if that
will work well.

There is an option to hide type definitions. This might be useful for beginners.
