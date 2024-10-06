import {
  writeFile,
  readFileToString,
  printFs,
  fs,
  unpackInto,
  pkgDir,
  writeFileInDir,
} from "./fs.ts";
import type { Compiler } from "./ulm-wasm.ts";
import { parseTarGzip } from "nanotar";
import type { Elm, ElmApp } from "./UlmEditor.elm";

/*

page loads:

needs to load codemirror, code-editor and elm
if either fails, render an error

needs to store inlined elm source code to wasi-fs

sends elm source code path as a flag {openFile: "/Main.elm"}

elm renders code-editor file="/Main.elm"

code-editor loads that string from wasi-fs and renders it
Can it set the editor to read-only?

code-editor starts loading ulm-wasm-compiler, elm dependencies

//*/

export function init(sourceFile: string) {
  const main = window.Elm.UlmEditor.init({
    node: document.getElementById("main"),
    flags: {
      file: sourceFile,
      theme: prefersDarkTheme() ? "dark" : "light",
    },
  });

  UlmEditor.elmApp = main;
  window.customElements.define("ulm-editor", UlmEditor);
}

function prefersDarkTheme(): boolean {
  // TODO load/store in LocalStorage
  return window?.matchMedia?.("(prefers-color-scheme:dark)")?.matches;
}

let ulm: Compiler | null = null;

async function initWasmCompiler() {
  if (ulm) return;

  const { loadCompiler } = await import("./ulm-wasm.ts");
  ulm = await loadCompiler(fs);
  await runElmInit();
  console.warn("loaded wasm-editor");
}

async function runElmInit() {
  // this tar file also contains the default `elm.json` (but it appears broken?)
  // const result = await fetch('/elm-init.tar.gz')
  // const tar = await parseTarGzip(await result.arrayBuffer())
  // await unpackInto('/', tar)
  // const result = await fetch("/elm-default-package-artifacts.tar.gz");
  const result = await fetch("/elm-all-examples-package-artifacts.tar.gz");
  const tar = await parseTarGzip(await result.arrayBuffer());
  console.log("tar", tar);
  await unpackInto({ dest: "/elm-home/0.19.1/packages", tar });

  await writeFile(
    "/elm.json",
    `{
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
          "elm/http": "2.0.0",
          "elm/random": "1.0.0",
          "elm/svg": "1.0.1",
          "elm/time": "1.0.0"
      },
      "indirect": {
          "elm/bytes": "1.0.8",
          "elm/file": "1.0.5",
          "elm/json": "1.1.3",
          "elm/url": "1.0.0",
          "elm/virtual-dom": "1.0.3"
      }
  },
  "test-dependencies": {
      "direct": {},
      "indirect": {}
  }
}
`,
  );

  await loadPackageRegistry();
}

async function loadPackageRegistry() {
  // TODO query https://package.elm-lang.org/all-packages
  // or query https://package.elm-lang.org/all-packages/since/{package-count}
  // then send it to wasm and store it in binary form?
  // -> See `fetch` in elm-compiler-wasm/builder/src/Deps/Registry.hs
  const allpackages = await fetch("/elm-home/0.19.1/packages/registry.dat");
  writeFileInDir(pkgDir, "registry.dat", await allpackages.arrayBuffer());
}

async function wip() {
  console.warn("🚧 ulm.wip");

  printFs();

  // TODO load from package proxy
  // https://github.com/elm-community/result-extra/archive/refs/tags/2.4.0.tar.gz
  const org = "elm-community";
  const pkg = "result-extra";
  const version = "2.4.0";
  const fetched = await fetch(
    `/package-proxy/elm-community/${pkg}/${version}.tar.gz`,
  );
  const tar = await parseTarGzip(await fetched.arrayBuffer());
  const dest = `/elm-home/0.19.1/packages/${org}/${pkg}`;
  console.info(
    `Downloaded package ${org}/${pkg}/${version} and will unpack it`,
    { dest, tar },
  );
  await unpackInto({
    dest,
    tar,
    replace: `${pkg}-${version}`,
    replaceValue: version,
  });

  printFs();

  // There is an issue with writing elm.json if the content is shorter than before:
  // The underlying ArrayBuffer of the virtual fs is not trimmed in size, and contains
  // additional data that makes it invalid.
  // I could try to fill it with spaces from wasm side similar to this?
  // fs.dir.contents.get("elm.json").data.fill(0x20);

  const result = await ulm.addPackage(`${org}/${pkg}`);

  console.warn("🚧 ulm.wip", result);
  console.log("!!elm.json", await readFileToString("/elm.json"));
}

async function compile(file: string) {
  if (!ulm) {
    console.error("Cannot compile, the WASM compiler was not loaded");
    return;
  }
  if (!file.trim()) {
    console.error(`Cannot compile, "${file}" is not a valid path`);
    return;
  }
  file = file.trim();

  const start = performance.now();
  console.info("Starting compilation of", file);
  // const result = await ulm.make(file)
  const result = await ulm.compile(await readFileToString(file));
  console.info(
    `Finished compilation after ${((performance.now() - start) / 1000).toFixed(3)}s`,
    result,
  );
  return result;
}

class UlmEditor extends HTMLElement {
  static observedAttributes = ["file", "theme"];
  static elmApp: ElmApp | null = null;

  private _source: string = "";
  private _theme: "light" | "dark" = "light";
  private _editor: unknown | null = null;
  private _file: string | null = null;
  private _isCompiling = false;
  private _lastBuild: URL | null = null;

  connectedCallback() {
    const sendChangeEvent = debounce(
      function () {
        const previous = this._source;
        this._source = this._editor.getValue();
        if (previous === this._source) return;
        writeFile(this._file, this._source);
        this.emit("change", null);
      }.bind(this),
    );

    //@ts-expect-error TODO import codemirror instead
    this._editor = window.CodeMirror(this, {
      mode: "elm",
      lineNumbers: true,
      keyMap: "sublime",
      matchBrackets: true,
      autoCloseBrackets: true,
      styleActiveLine: true,
      theme: this._theme,
      value: this._source,
      tabSize: 2,
      indentWithTabs: false,
      extraKeys: {
        Esc: (cm) => {
          console.warn("TODO escape focus trap");
        },
        Tab: (cm) => {
          cm.execCommand("indentMore");
        },
        "Shift-Tab": (cm) => {
          cm.execCommand("indentLess");
        },
        "Cmd-S": (cm) => {
          this.emit("save", null);
        },
        // "Ctrl-Enter": (cm) => { this.emit('save', null) }
        "Ctrl-Enter": (cm) => {
          this.compile();
        },
      },
    });
    // this._editor.on('changes', this.sendChangeEvent.bind(this));
    // this._editor.on("changes", (evt) => console.log("editor changes", evt));

    this._isCompiling = false;
    this.connectPorts();

    requestIdleCallback(() => {
      this.dispatchEvent(new CustomEvent(""));
      initWasmCompiler()
        .then(() => {
          this.emit("compiler", "ready");
          this.compile();
        })
        .catch((error) => {
          this.emit("compiler", { error });
        });
    });

    this.addEventListener("compile-result", (evt: CustomEvent) =>
      console.warn("change-result", evt.detail),
    );
  }

  disconnectedCallback() {
    this._editor = null;
    this._source = "";
    this._isCompiling = false;
    this._lastBuild = null;
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    console.log(`Attribute ${name} has changed.`, { oldValue, newValue });

    switch (name) {
      case "file":
        this._file = newValue;
        if (newValue && newValue !== oldValue) {
          readFileToString(newValue).then((content) => {
            console.info(`Loaded file '${newValue}' from fs`);
            this._source = content;
            this._editor?.setValue(content);
          });
        }
        break;
      case "theme":
        this._editor?.setOption("theme", newValue);

        break;
      default:
        console.warn(`TODO handle attribute "${name}" change`);
    }
  }

  private emit(event: string, detail: unknown) {
    this.dispatchEvent(new CustomEvent(event, { bubbles: true, detail }));
  }

  private connectPorts() {
    if (!UlmEditor.elmApp) {
      throw new Error("No Elm App was attached to the editor");
    }
    UlmEditor.elmApp.ports.interopFromElm.subscribe((msg) => {
      console.info("port message `fromElm`", msg);
      switch (msg.tag) {
        case "revoke-object-url":
          return URL.revokeObjectURL(msg.data);
        case "compile":
          this.compile();
          break;
        case "replace-code":
          this._editor?.setValue(msg.data);
          this.compile();
          break;
        case "wip-js":
          return wip();
        default:
          console.warn("Unknown port message `fromElm`", msg);
      }
    });
  }

  private async compile() {
    if (this._isCompiling) {
      console.warn("Skipping compilation because it is already in progress");
      return;
    }
    if (!this._file || !this._editor) {
      console.warn("Skipping compilation because file or editor are not set");
      return;
    }
    this._isCompiling = true;
    try {
      writeFile(this._file, this._editor.getValue());
      const result = await compile(this._file ?? "");
      if (result?.type === "success") {
        const data = await readFileToString(result.file);
        const html = wrapInHtml(data, result.name);
        const blob = new Blob([html], { type: "text/html" });
        const url = URL.createObjectURL(blob);
        this.emit("compile-result", { type: "success", url });
      } else {
        this.emit("compile-result", result);
      }
    } catch (ex) {
    } finally {
      this._isCompiling = false;
    }
  }
}

export function wrapInHtml(code: string, name: string) {
  return `<!DOCTYPE HTML>
<html>
<head>
    <meta charset="UTF-8">
    <title>${name}</title>
    <style>body { padding: 0; margin: 0; }</style>
</head>

<body>

<pre id="elm"></pre>

<script>
try {
${code}

    var app = Elm.${name}.init({ node: document.getElementById("elm") });
}
catch (e)
{
    // display initialization errors (e.g. bad flags, infinite recursion)
    var header = document.createElement("h1");
    header.style.fontFamily = "monospace";
    header.innerText = "Initialization Error";
    var pre = document.getElementById("elm");
    document.body.insertBefore(header, pre);
    pre.innerText = e;
    throw e;
}
</script>

</body>
</html>`;
}

// Not yet supported in Safari 17.6
window.requestIdleCallback =
  window.requestIdleCallback ||
  function requestIdleCallbackFallback(cb) {
    var start = Date.now();
    return setTimeout(function () {
      cb({
        didTimeout: false,
        timeRemaining: function () {
          return Math.max(0, 50 - (Date.now() - start));
        },
      });
    }, 1);
  };

// Not yet supported in Safari 17.6
window.cancelIdleCallback =
  window.cancelIdleCallback ||
  function cancelIdleCallbackFallback(id) {
    clearTimeout(id);
  };

// DEBOUNCER

function debounce(func: Function) {
  let token: number | null = null;
  return () => {
    function later() {
      token = null;
      func.apply(null, arguments);
    }
    if (token) cancelIdleCallback(token);
    token = requestIdleCallback(later);
  };
}

export { UlmEditor, writeFile, printFs };
