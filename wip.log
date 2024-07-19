Another fresh start, see older log files [log2](../mdgriffith_elm-dev/ext-ulm/log.md) and before that [log1]([marc.md](../elm-js-compiler/marc.md) for more info and things I tried.

Now I could try to load an `elm.json` file from wasm (to find out about dependencies), or instead get started on compiling `elm-community/result-extra` into an `artifacts.dat`.

So I'm trying to load an `elm.json` file now from the current directory.

I started inside `builder/src/Elm/Details.hs` with the `load` function, but I don't know if getting the file modification time of `elm.json` will be possible. So I'm trying to skipt that for now.
I will also skip reading `Stuff.details` from `./elm-stuff/0.19.1/elm.d` which contains info about the last run build (like time), so I can skip ahead to the `generate` fn in `Details.hs` because that will always be executed if something needs to be compiled.

The `generate` fn calls `initEnv`, which uses `Solver.initEnv` and contacts the elm package server (so it won't work directly in WASM).  
So I'll try to only extract `Outline.read` (which reads the contents of the elm.json file) from there and then run `verifyApp`.
Maybe I'll need to fake a valid environment that knows about packages (what `builder/src/Deps/Sover` does). But first I will try to ignore that.

From there I only want to use `verifyDependencies` in general, and specificially `build` that creates `artifacts.dat` files.  
As a first test, I try to build the dependency `elm-community/maybe-extra/5.3.0` which has no indirect dependencies and only one file.

---

I removed everything and started again from the top with `Details.load`.  
I cloned `builder/src/Elm/Details.hs` to `Ulm/Details.hs` and then copied everything that was necessary.

I skipped running `build` so far by always providing a pre-built `artifacts.dat` for every dependency from my own ELM_HOME dir.

Now I want to get `build` working. First with the `elm-community/maybe-extra/5.3.0` package.

For `build` I hardly had to do anything. I copied from the elm compiler and removed a few things, but then I was able to compile maybe-extra.

---

After `Details.load` now works, I want to to reuse parts of that code to replace the hardcoded `Ulm.ReadArtifacts` code.  
That code might still be useful for a REPL or another editor that does not allow to install custom packages.

But now I want to prepare building all dependencies to artifacts (and returning them) so I can start using that inside welmo (forked https://elm-lang.org/try editor).

I added a `Ulm.Details.loadArtifactsForApp` function so I don't need to write all `artifacts.dat` files first and then load them again, but rather return the artifacts, after compiling the dependencies.

I then used this function inside `Ulm.compileWasm` to no longer need hardcoded dependencies, but rather load them from "disk".

---

With this shortcut in place to build one source file and its dependencies, I want to follow the proper trail of `elm make` to build the dependencies and then one file with a main (and possible additional local files).

Starting with `runHelp` in elm-compiler-wasm/terminal/src/Make.hs I can first use `Details.load` to ensure that all dependencies are built and loaded, then skip ahead to `buildPaths` and then pick the path to compile a js file (which contains at least one `main` function).

Note: I stopped with integrating `Make` fully and instead switched to getting a more minimal solution like the elm-lang.org/try editor or ellie working.  
I will come back to building multipler user-generated Elm files after that.

---

Because I can't download zipballs from github due to CORS issues. I will
have to use a different way to pull in dependencies.

Right now, I think tarballs will serve me best that I will need to store
somewhere, but can decode easily in the browser with the lightweight
nanotar dependency and the DecompressionStream API.

To test this, I ran `elm init` which generates such an elm.json file (as of 2024-07-13)

```json
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0",
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
```
and created a `elm-init.tar.gz` of the directory content including packages (without `artifacts.dat` and `docs.json` files).

Now I will load that tarball and create the virtual fs with that data.

And hope that I can still compile the static sample code.

---

It turned out I was not actually compiling the dependencies, but only ran the check on them and then used the precompiled `artifacts.dat` files.

That needs to change.

I now have problems compiling indirect dependencies, it seems like I need to add the file lock mechanism (or ensure that all dependencies are built sequentially (I guess remove calls to `fork`)

```
[WASI stderr] : /elm-home/0.19.1/packages/elm/time/1.0.0/docs.json: withBinaryFile: resource busy (file is locked)
[WASI stderr] HasCallStack backtrace:
[WASI stderr]   collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:92:13 in ghc-internal:GHC.Internal.Exception
[WASI stderr]   toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/IO.hs:260:11 in ghc-internal:GHC.Internal.IO
[WASI stderr]   throwIO, called at libraries/ghc-internal/src/GHC/Internal/IO/Exception.hs:315:19 in ghc-internal:GHC.Internal.IO.Exception
[WASI stderr]   ioException, called at libraries/ghc-internal/src/GHC/Internal/IO/Exception.hs:319:20 in ghc-internal:GHC.Internal.IO.Exception
```

These errors occur from time to time, and they are reduced if I pre-create the `docs.json` and `artifacts.dat` files as empty files.

And most times, pressing the "Compile code" button again is enough.

So maybe I'll avoid this (for now?) by triggering compilation of all dependencies individually? And I guess with the indirect ones first, because they are nested deps.
If one such build fails, I can trigger it again if I can catch the exception somewhere?

---

I also checked the file size of prebuilt bundles, see also ./default-packages/README.md 

1. The default packages that elm-init installs (only source files) `default-packages.tar.gz` is 193.3KiB
2. If I also include http, random and url `default-with-http-random-url.tar.gz` grows to 230.3KiB
3. When I include both sources and built `artifacts.dat` and `docs.json` files, `with-sources-and-artifacts.tar.gz` takes 403.3KiB
4. If I remove the Elm and js source files from that, it is only 256.1KiB
5. And if I remove http and random (and their deps) from that, `artifacts-and-docs.without-http-random.tar.gz` shrinks to 214.7KiB (same packages as in 1)

So my approach for the editor will be this:
Load the default packages as artifacts and docs (I want to add a docs viewer later)
And then allow the user to install (and prebuild) new elm packages from source tarballs.

These source tarballs will be stored by the cache-server because of the CORS issues mentioned above. More in [./docs/cache-server.md](./docs/cache-server.md).

I decided to only include the `artifacts.dat` files, an empty `src` dir and `elm.json` for each package (those are what the compiler is looking for), and then the `elm-default-package-artifacts.tar.gz` is only 105KiB big.

The `docs.json` files of those packages will be loaded when they are needed. Either when trying to open the (yet to fork) package doc viewer, or in the service worker for offline usage.

