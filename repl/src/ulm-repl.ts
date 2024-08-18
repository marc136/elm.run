import {
  writeFile,
  readFileToString,
  printFs,
  fs,
  unpackInto,
  pkgDir,
  writeFileInDir,
} from "../../ulm-editor/src/fs.ts";
import type { Compiler } from "./ulm-wasm.ts";
import { parseTarGzip } from "nanotar";
import type { Elm, ElmApp } from "./UlmRepl.elm";

export function init(sourceFile: string) {
  const main = window.Elm.UlmRepl.init({
    node: document.getElementById("main"),
    flags: {
      file: sourceFile,
      theme: prefersDarkTheme() ? "dark" : "light",
    },
  });

  ReplInput.elmApp = main;
  window.customElements.define("ulm-editor", ReplInput);
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
  const result = await fetch("/elm-all-examples-package-artifacts.tar.gz");
  const tar = await parseTarGzip(await result.arrayBuffer());
  console.log("tar", tar);
  await unpackInto("/elm-home/0.19.1/packages", tar);

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
                "elm/core": "1.0.5",
                "elm/json": "1.1.3"
            },
            "indirect": {
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

type ReplResult =
  | { type: "new-import"; data: string /* name */ }
  | { type: "new-type"; data: string /** name */ }
  | { type: "new-work"; input: string; output: string }
  | { type: "do-nothing"; data: "undefined" }
  | { type: "no-ports"; data: "undefined" }
  | { type: "compile-errors"; errors: unknown[]; input: string }
  | {
      type: "error";
      input: string;
      path: string;
      title: string;
      message: string;
    };

async function repl(code: string): Promise<ReplResult> {
  if (!ulm) {
    const msg = "Cannot compile, the WASM compiler was not loaded";
    console.error(msg);
    return Promise.reject(msg);
  }
  code = code.trim();
  // printFs();
  const start = Date.now();
  console.info("Read-eval-print", code);
  // const result = await ulm.make(file)
  const compiled = await ulm.wip(code);
  console.info(
    `Finished after ${((Date.now() - start) / 1000).toFixed(3)}s`,
    compiled,
  );
  const result = JSON.parse(compiled);
  // dynamic import of an ESM
  // const encoded = btoa(unescape(encodeURIComponent(code)));
  // const module = await import("data:text/javascript;base64," + encoded);
  // const computed = module.default();
  console.info("result:", result);
  switch (result.type) {
    case "new-work":
      return new Promise((resolve, reject) => {
        // data is a string wrapped in a string like `'"function () {...}"'`
        const blob = new Blob([JSON.parse(result.data)], {
          type: "text/javascript",
        });
        const url = URL.createObjectURL(blob);
        const worker = new Worker(url, { type: "classic" });

        const timeoutAfterSeconds = 30;
        const timeout = setTimeout(() => {
          const msg = `Running compiled Elm code timed out after %{timeoutAfterSeconds}s`;
          console.error(msg);
          worker.terminate();
          reject(msg);
        }, timeoutAfterSeconds * 1000);

        worker.onmessage = function (e) {
          clearTimeout(timeout);
          resolve({ type: "new-work", input: code, output: e.data });
        };

        worker.onerror = function (e) {
          clearTimeout(timeout);
          console.error("Error in worker:", e);
          reject(e);
        };

        URL.revokeObjectURL(url);
      });

    case "new-import":
    case "new-type":
    case "do-nothing":
    case "no-ports":
      return result;

    case "compile-errors":
    case "errors":
      result.input = code;
      return result;

    default:
      console.error(`Cannot handle "${result.type}"`, result);
      throw result;
  }
}

class ReplInput extends HTMLElement {
  static observedAttributes = ["file", "theme"];
  static elmApp: ElmApp | null = null;

  private _source: string = "";
  private _theme: "light" | "dark" = "light";
  private _editor: unknown | null = null;
  private _file: string | null = null;
  private _isCompiling = false;
  private _lastBuild: URL | null = null;

  connectedCallback() {
    this.addEventListener("compile-result", (evt: CustomEvent) =>
      console.error("compile-result", evt.detail),
    );

    const sendChangeEvent = debounce(
      function () {
        console.log("sendChangeEvent");
        const previous = this._source;
        this._source = this._editor.getValue();
        if (previous === this._source) return;
        // compile and run
        // if code ends with `\n\n` we force compilation
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
          this.run();
        },
      },
    });
    this._editor.on("changes", sendChangeEvent.bind(this));
    // this._editor.on("changes", (evt) => console.log("editor changes", evt));

    this._isCompiling = false;
    this.connectPorts();

    requestIdleCallback(() => {
      this.emit("compiler", "loading");
      initWasmCompiler()
        .then(() => {
          this.emit("compiler", "ready");
          this.run();
        })
        .catch((error) => {
          this.emit("compiler", { error });
        });
    });
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
    if (!ReplInput.elmApp) {
      throw new Error("No Elm App was attached to the editor");
    }
    if (ReplInput.elmApp.ports.interopFromElm) {
      ReplInput.elmApp.ports.interopFromElm.subscribe((msg) => {
        console.info("port message `fromElm`", msg);
        switch (msg.tag) {
          case "revoke-object-url":
            return URL.revokeObjectURL(msg.data);
          case "compile":
            this.run();
            break;
          case "replace-code":
            this._editor?.setValue(msg.data);
            this.run();
            break;
          default:
            console.warn("Unknown port message `fromElm`", msg);
        }
      });
    } else {
      console.warn("interopFromElm was not attached");
    }
  }

  private async run() {
    if (this._isCompiling) {
      console.warn("Skipping compilation because it is already in progress");
      return;
    }
    if (!this._editor) {
      console.warn("Skipping comilation because editor is not set");
      return;
    }
    this._isCompiling = true;
    try {
      const code = this._editor.getValue();
      writeFile(this._file, code);
      const result = await repl(code ?? "");
      this.emit("compile-result", result);
    } catch (ex) {
      console.error("compile failed", ex);
    } finally {
      this._isCompiling = false;
    }
  }
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

export { ReplInput as UlmEditor, writeFile, printFs };
