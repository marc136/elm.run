# Elm compilers

In the last weeks, two full rewrites of the Elm compiler in Elm itself have surfaced.\

I'm not sure yet which project will work better of the different tools I want to have on [elm.run](https://elm.run), but the wasm build is maybe too heavyweight for a tour or the repl in comparison.

The elm-in-elm rewrites might also make it easier to [add translated error messages](https://github.com/katjam/local-elm/issues/5), which I'm interested in.

- [.marc136](https://github.com/marc136/elm-compiler-wasm)
- [./pithub/](https://github.com/pithub/elm-compiler-in-elm-ui)
- [./guida/](https://github.com/guida-lang/compiler)

# Other Elm forks
There are several other interesting forks of the official Elm compiler, I'll try to keep a list here:

- [mdgriffith/elm-dev](https://github.com/mdgriffith/elm-dev) runs in watch mode and can be used to query information about the Elm code, like "what is the inferred type of this declaration?"
- [lamdera](https://github.com/lamdera/compiler) powers https://lamdera.com/ and also https://elm-pages.com/
- [zokka](https://github.com/Zokka-Dev/zokka-compiler/) adds support for [custom package servers](https://github.com/Zokka-Dev/zokka-custom-package-server) and bugfixes.
- [pithub/elm-compiler-in-elm-ui](https://github.com/pithub/elm-compiler-in-elm-ui) is a port of the Elm compiler and its cli to Elm and runs in the browser, see [demo video (2024-08)](https://www.youtube.com/watch?v=OK9S_HUdReA).
- [guida-lang/compiler](https://github.com/guida-lang/compiler) is a port of the Elm compiler and its cli to Elm and runs in node.js.
