# Ulm WASM REPL

From view-source:https://guide.elm-lang.org/gitbook/gitbook-plugin-elm-repl/repl.js

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


The REPL on guide.elm-lang.org is a closed-source Elm program.
On every [enter] key press, it sends the input to the server which responds either with a command like `indent` or with js code.

For example

```
{"imports":{},"types":{"User":"type alias User = { name : String, age : Int }\n"},"decls":{},"entry":"if 13 > 12 then\n  \"12\"\n  else\n  \"horst\""}
```

returns `indent`

And a second press to [enter] will then return the js code to calculate and print the result.

```
{"imports":{},"types":{"User":"type alias User = { name : String, age : Int }\n"},"decls":{},"entry":"if 13 > 12 then\n  \"12\"\n  else\n  \"horst\"\n  "}
```

See ../elm-compiler-wasm/worker/src/Endpoint/Repl.hs to get started

In the meantime, elmrepl.de has gotten a lot faster (it is not sponsored by netcup.de).

