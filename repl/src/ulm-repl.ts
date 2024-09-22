import { fs, printFs, unpackInto, writeFile } from "../../ulm-editor/src/fs.ts";
import type { ReplCompiler } from "./repl-wasm.ts";
import { parseTarGzip } from "nanotar";
import type { ElmApp } from "./UlmRepl.elm";
import {
  ColorSchemeSelector,
  detectPreferredColorScheme,
  type Scheme,
} from "./scheme-selector.ts";
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

  ReplInput.elmApp = main;
  window.customElements.define("ulm-editor", ReplInput);
  window.customElements.define("scheme-selector", ColorSchemeSelector);
  window.customElements.define("modal-dialog", ModalDialog);

  requestAnimationFrame(detectStickyElements);
}

function detectStickyElements() {
  // Used for sticky top navigation, see https://stackoverflow.com/a/57991537
  const observer = new IntersectionObserver(
    ([e]) => e.target.classList.toggle("is-stuck", e.intersectionRatio < 1),
    { threshold: [1], rootMargin: "-1px 0px 0px 0px" },
  );
  document.querySelectorAll(".sticky").forEach((elem) =>
    observer.observe(elem)
  );
}

let ulm: ReplCompiler | null = null;

async function initWasmCompiler() {
  if (ulm) return;

  const { loadCompiler } = await import("./repl-wasm.ts");
  ulm = await loadCompiler(fs);
  await runElmInit();
  console.info("initWasmCompiler() successful");
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

type ReplResult =
  | { type: "new-import"; name: string }
  | { type: "new-type"; name: string }
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
  const start = performance.now();
  console.info("Read-eval-print", code);
  const compile = persistState ? ulm.evaluate : ulm.check;
  const result = await compile(code);
  console.info(
    `Compiled in ${((performance.now() - start) / 1000).toFixed(3)}s`,
    { input: code, result },
  );
  // dynamic import of an ESM
  // const encoded = btoa(unescape(encodeURIComponent(code)));
  // const module = await import("data:text/javascript;base64," + encoded);
  // const computed = module.default();
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
      return {
        type: result.type,
        name: JSON.parse(result.name),
      };

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

type Evaluated = { name: null | string; value: string; type: string };
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
      console.warn(`evaluateJs ${typeof e.data} result`, e.data);
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

const examples = [
  [
    "1 + 1",
    "double : Int -> Int\ndouble number =\n    2 * number",
    "double 1",
  ],
  [
    "String.repeat",
    "again : String -> String\nagain =\n    String.repeat 2",
    'again "Hello"',
  ],
  [
    "import Dict exposing (Dict)",
    "Dict.empty",
    'Dict.empty\n    |> Dict.insert "one" 1\n    |> Dict.insert "pi" Basics.pi',
  ],
  [
    "type alias Tree =\n    { name: String\n    }",
    'elm = Tree "Elm"',
    "noTree : Tree\nnoTree =\n    { name = 7\n    }",
    'aTree : Tree\naTree =\n    { name = "Elm"\n    }',
  ],
];

async function evaluateExamples(elmApp: ElmApp) {
  const exampleIndex = Math.floor(Math.random() * examples.length);
  console.log("Will show examples", exampleIndex);
  const chosen = examples[exampleIndex];
  for (const input of chosen) {
    const event = {
      tag: "evaluated",
      id: performance.now(),
      input,
      result: await repl(input, true),
    };
    elmApp.ports.interopToElm.send(event);
  }
}

class ReplInput extends HTMLElement {
  static observedAttributes = ["file", "theme"];
  static elmApp: ElmApp | null = null;

  private _source: string = "";
  private _theme: Scheme = "light";
  private _editor: unknown | null = null;

  private _currentlyCompiling: EpochTimeStamp = 0;
  /** To detect after a compilation run if we need to start another */
  private _lastChange: EpochTimeStamp = 0;

  connectedCallback() {
    this._theme = this.getAttribute("theme") as Scheme;

    //@ts-expect-error TODO import codemirror instead
    this._editor = window.CodeMirror(this, {
      autofocus: this.hasAttribute("autofocus") ?? false,
      mode: "elm",
      lineNumbers: false,
      keyMap: "sublime",
      matchBrackets: true,
      autoCloseBrackets: true,
      screenReaderLabel: "Enter Elm code here to evaluate it",
      styleActiveLine: true,
      theme: this._theme,
      value: this._source || 'elm = "delightful"',
      tabSize: 4,
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
        "Ctrl-Enter": (cm) => {
          this.evaluate();
        },
      },
    });
    this._editor.on("changes", this.check.bind(this));

    this._lastChange = 0;
    this._currentlyCompiling = 0;
    this.connectPorts();

    requestIdleCallback(() => {
      this.emit("compiler", "loading");
      initWasmCompiler()
        .then(() => {
          console.log("compiler ready");
          this.emit("compiler", "ready");
          this.check();

          if (location.hash.includes("example") && ReplInput.elmApp) {
            evaluateExamples(ReplInput.elmApp);
          }
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
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    console.log(`Attribute ${name} has changed.`, { oldValue, newValue });

    switch (name) {
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
    const elm = ReplInput.elmApp;
    if (!elm) {
      throw new Error("No Elm App was attached to the editor");
    }
    if (elm.ports.interopFromElm) {
      elm.ports.interopFromElm.subscribe(async (msg) => {
        console.info("port message `fromElm`", msg);
        switch (msg.tag) {
          case "compile":
            this.evaluate();
            break;
          case "enter-example-code":
            evaluateExamples(elm);
            break;
          case "replace-entered-code":
            this._editor?.setValue(msg.data);
            this._editor?.focus();
            this.check();
            scrollToBottom();
            break;
          case "scroll-to-bottom":
            scrollToBottom();
            break;
          case "remove-from-state":
            if (Array.isArray(msg.data)) {
              msg.data.forEach((name) => ulm?.removeFromState(name));
            } else {
              console.error("Expected a list of strings to remove from state");
            }
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
    const result = { tag: "checked-input", ...tried.result };
    ReplInput.elmApp?.ports.interopToElm.send(result);
  }

  private async evaluate() {
    const compiled = await this.tryCompile(true);
    if (!compiled) return;

    switch (compiled.result.type) {
      case "new-decl":
      case "evaluate":
      case "evaluated":
      case "new-import":
      case "new-type":
        if (this._editor && this._editor.getValue().trim() == compiled.input) {
          this._editor.setValue("");
        }
        break;

      case "do-nothing":
      case "no-ports":
      case "compile-errors":
      case "errors":
      default:
        break;
    }

    const result = { tag: "evaluated", ...compiled };
    console.info("Sending to Elm:", result);
    ReplInput.elmApp?.ports.interopToElm.send(result);
    return result;
  }

  private async tryCompile(persistState: boolean = false) {
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
        return { input: code, id: now, result };
      } else {
        // To clear the type hint when input was fully deleted
        return { input: "", id: now, result: { type: "nothing" } };
      }
    } catch (ex) {
      console.error("compile failed", ex);
    } finally {
      if (this._lastChange > this._currentlyCompiling) {
        // schedule a new build when idle
        requestIdleCallback(() => this.tryCompile(persistState));
      }
      this._currentlyCompiling = 0;
    }
  }
}

async function scrollToBottom() {
  return new Promise((resolve) => {
    requestAnimationFrame(() => {
      window.scrollTo(0, document.body.scrollHeight);
      return resolve(null);
    });
  });
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

export { ColorSchemeSelector, printFs, ReplInput as UlmEditor, writeFile };
