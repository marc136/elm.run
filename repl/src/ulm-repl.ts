import {
  fs,
  printFs,
  readFileToString,
  unpackInto,
  writeFile,
} from "../../ulm-editor/src/fs.ts";
import type { Compiler } from "./repl-wasm.ts";
import { parseTarGzip } from "nanotar";
import type { Elm, ElmApp } from "./UlmRepl.elm";
import { detectPreferredColorScheme, ColorSchemeSelector, type Scheme } from "./theme-selector.ts";
import { ModalDialog } from "./ModalDialog.ts";

document.documentElement.dataset.theme = detectPreferredColorScheme();

export function init(sourceFile: string) {
  const main = window.Elm.UlmRepl.init({
    node: document.getElementById("main"),
    flags: {
      file: sourceFile,
      theme: detectPreferredColorScheme(),
    },
  });

  setTimeout(() => {
    console.warn('DEBUG: Adding history from js')
    const entries = [
      {
        "input": "String.repeat",
        "id": 16804,
        "result": {
          "type": "evaluated",
          "value": {
            "name": null,
            "value": "\u001b[36m<function>\u001b[0m",
            "type": "Int -> String -> String"
          }
        }
      },
      {
        "input": "abc =\n  String.repeat",
        "id": 17171,
        "result": {
          "type": "new-decl",
          "name": "abc",
          "value": {
            "name": "abc",
            "value": "\u001b[36m<function>\u001b[0m",
            "type": "Int -> String -> String"
          }
        }
      },
      {
        "input": "abc : Int -> String -> String\nabc =\n  String.repeat",
        "id": 31396,
        "result": {
          "type": "new-decl",
          "name": "abc",
          "value": {
            "name": "abc",
            "value": "\u001b[36m<function>\u001b[0m",
            "type": "Int -> String -> String"
          }
        }
      },
    ];
    entries.forEach(entry => {
      main.ports.interopToElm.send({ tag: 'evaluated', ...entry })
    })

  }, 200);

  ReplInput.elmApp = main;
  window.customElements.define("ulm-editor", ReplInput);
  window.customElements.define("scheme-selector", ColorSchemeSelector);
  window.customElements.define("modal-dialog", ModalDialog);

  requestAnimationFrame(detectStickyElements)
}

function detectStickyElements() {
  // Used for sticky top navigation, see https://stackoverflow.com/a/57991537 
  const observer = new IntersectionObserver(
    ([e]) => e.target.classList.toggle('is-stuck', e.intersectionRatio < 1),
    { threshold: [1], rootMargin: '-1px 0px 0px 0px' }
  );
  document.querySelectorAll('.sticky').forEach(elem => observer.observe(elem));
}

let ulm: Compiler | null = null;

async function initWasmCompiler() {
  if (ulm) return;

  const { loadCompiler } = await import("./repl-wasm.ts");
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

  // await loadPackageRegistry();
}

type ReplWasm = {
  input: string;
  started: EpochTimeStamp;
  result: ReplResult;
};

type ReplResult =
  | { type: "new-import"; data: string /* name */ }
  | { type: "new-type"; data: string /** name */ }
  | { type: "new-decl"; name: string; value: Evaluated }
  | { type: "evaluated"; value: Evaluated }
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


async function repl(code: string, persistState: boolean): Promise<ReplResult> {
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
  const compile = persistState ? ulm.evaluate : ulm.check
  const compiled = await compile(code);
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
    case "new-decl":
      return {
        type: result.type,
        name: JSON.parse(result.name),
        value: await evaluateJs(result.evaluate),
      };

    case "evaluate":
      return {
        type: "evaluated",
        value: await evaluateJs(result.evaluate),
      };

    case "new-import":
    case "new-type":
    case "do-nothing":
    case "no-ports":
      return result;

    case "compile-errors":
    case "errors":
      // result.input = code;
      return result;

    default:
      console.error(`Cannot handle "${result.type}"`, result);
      throw result;
  }
}

type Evaluated = { name: null | string, value: string, type: string }
async function evaluateJs(code: string): Promise<Evaluated> {
  return new Promise((resolve, reject) => {
    // data is a string wrapped in a string like `'"function () {...}"'`
    const blob = new Blob([JSON.parse(code)], {
      type: "text/javascript",
    });
    const url = URL.createObjectURL(blob);
    const worker = new Worker(url, { type: "classic" });

    const timeoutAfterSeconds = 15;
    const timeout = setTimeout(() => {
      const msg =
        `Running compiled Elm code timed out after %{timeoutAfterSeconds}s`;
      console.error(msg);
      worker.terminate();
      reject(msg);
    }, timeoutAfterSeconds * 1000);

    worker.onmessage = function (e) {
      clearTimeout(timeout);
      console.warn(`evaluateJs ${typeof e.data} result`, e.data)
      resolve(e.data);
    };

    worker.onerror = function (e) {
      clearTimeout(timeout);
      console.error("Error in worker:", e);
      reject(e);
    };

    URL.revokeObjectURL(url);
  });
}

class ReplInput extends HTMLElement {
  static observedAttributes = ["file", "theme"];
  static elmApp: ElmApp | null = null;

  private _source: string = "";
  private _theme: Scheme = "light";
  private _editor: unknown | null = null;
  private _file: string | null = null;
  private _lastBuild: URL | null = null;

  private _currentlyCompiling: EpochTimeStamp = 0;
  /** To detect after a compilation run if we need to start another */
  private _lastChange: EpochTimeStamp = 0;

  connectedCallback() {
    this.addEventListener(
      "compile-result",
      (evt: CustomEvent) => console.error("compile-result", evt.detail),
    );

    this._theme = this.getAttribute('theme') as Scheme;

    //@ts-expect-error TODO import codemirror instead
    this._editor = window.CodeMirror(this, {
      autofocus: this.hasAttribute('autofocus') ?? false,
      mode: "elm",
      lineNumbers: false,
      keyMap: "sublime",
      matchBrackets: true,
      autoCloseBrackets: true,
      screenReaderLabel: 'Enter Elm code here to evaluate it',
      styleActiveLine: true,
      theme: this._theme,
      value: this._source || '1 + 2',
      tabSize: 4,
      indentWithTabs: false,
      extraKeys: {
        Esc: (cm) => {
          console.warn('TODO escape focus trap')
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
    this._editor.on("changes", this.check.bind(this));
    // this._editor.on("changes", (evt) => console.log("editor changes", evt));

    this._lastChange = 0;
    this._currentlyCompiling = 0;
    this.connectPorts();

    requestIdleCallback(() => {
      this.emit("compiler", "loading");
      initWasmCompiler()
        .then(() => {
          console.log('compiler ready')
          this.emit("compiler", "ready");
          this.check();
        })
        .catch((error) => {
          this.emit("compiler", { error });
        });
    });
  }

  disconnectedCallback() {
    this._editor = null;
    this._source = "";
    this._currentlyCompiling = 0;
    this._lastChange = 0;
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
      ReplInput.elmApp.ports.interopFromElm.subscribe(async (msg) => {
        console.info("port message `fromElm`", msg);
        switch (msg.tag) {
          case "compile":
            this.compile();
            break;
          case "enter-example-code":
            const examples = [
              'type alias Tree =\n    { name: String\n    }',
              'elm = Tree "Elm"',
              '1',
              'double : Int -> Int\ndouble number =\n    2 * number',
              'double 1',
              'double 21',
              'String.repeat',
              'again : String -> String\nagain =\n    String.repeat 2',
              'again "Hello"',
            ];
            for (const input of examples) {
              const event = {
                tag: 'evaluated',
                id: performance.now(),
                input,
                result: await repl(input, true),
              }
              // TODO extract into one function that is used by `compile`, too?
              ReplInput.elmApp?.ports.interopToElm.send(event);
            }
            break;
          case "replace-entered-code":
            this._editor?.setValue(msg.data);
            this._editor?.focus();
            this.check();
            break;
          case 'scroll-to-bottom':
            requestAnimationFrame(() => window.scrollTo(0, document.body.scrollHeight));
            break;
          case 'remove-from-state':
            if (Array.isArray(msg.data)) {
              msg.data.forEach(name => ulm?.removeFromState(name));
            } else {
              console.error('Expected a list of strings to remove from state');
            };
            break;
          default:
            console.warn("Unknown port message `fromElm`", msg);
        }
      });
    } else {
      console.warn("interopFromElm was not attached");
    }
  }

  private async check() {
    const tried = await this.tryCompile(false);
    if (!tried?.result) return;
    console.info({ tag: "checked-input", ...tried.result }, tried)
    ReplInput.elmApp?.ports.interopToElm.send({ tag: "checked-input", ...tried.result });
  }

  private async compile() {
    const compiled = await this.tryCompile(true);
    if (!compiled) return;
    // this.emit("compile-result", result);
    console.warn('compile-result', compiled);

    switch (compiled.result.type) {
      case "new-decl":
      case "evaluate":
      case "evaluated":
      case "new-import":
      case "new-type":
        if (this._editor && this._editor.getValue().trim() == compiled.input) {
          this._editor.setValue('')
        }
        break;

      case "do-nothing":
      case "no-ports":
      case "compile-errors":
      case "errors":
      default:
        break;
    }

    const result = { tag: 'evaluated', ...compiled };
    ReplInput.elmApp?.ports.interopToElm.send(result);
    return result;
  }

  private async tryCompile(persistState: false) {
    if (!this._editor) {
      console.warn("Skipping comilation because editor is not set");
      return;
    }

    const now = performance.now();
    this._lastChange = now;
    if (this._currentlyCompiling > 0) return;
    this._currentlyCompiling = now;

    try {
      const code: string = this._editor?.getValue().trim();
      if (code) {
        const result = await repl(code, persistState);
        // this.emit("compile-result", result);
        return { input: code, id: now, result };
      } else {
        // To clear the type hint when input was fully deleted
        return { input: '', id: now, result: { type: 'nothing' } }
      }
    } catch (ex) {
      console.error("compile failed", ex);
    } finally {
      if (this._lastChange > this._currentlyCompiling) {
        // schedule a new build when idle
        requestIdleCallback(this.tryCompile.bind(this));
      }
      this._currentlyCompiling = 0;
    }
  }
}

// Not yet supported in Safari 17.6
window.requestIdleCallback = window.requestIdleCallback ||
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
window.cancelIdleCallback = window.cancelIdleCallback ||
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

export { printFs, ReplInput as UlmEditor, writeFile, ColorSchemeSelector };
