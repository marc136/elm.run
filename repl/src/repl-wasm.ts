import { ConsoleStdout, File, OpenFile, WASI } from "@bjorn3/browser_wasi_shim";
import type { Fd } from "@bjorn3/browser_wasi_shim";

// TODO extract this into the elm-compiler-wasm repo

import ulmJs from "../../www/generated/repl";

export interface ReplCompiler {
  evaluate: (source: string) => Promise<CompileResult>;
  check: (source: string) => Promise<CompileResult>;
  removeFromState: (name: string) => Promise<null>;
}

export type CompileResult =
  | { type: "new-import"; name: string }
  | { type: "new-type"; name: string }
  | { type: "new-decl"; name: string; evaluate: string }
  | { type: "evaluate"; evaluate: string }
  | { type: "do-nothing"; data: "undefined" }
  | { type: "no-ports"; data: "undefined" }
  | {
    type: "compile-errors";
    // CompilerReport in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
    errors: unknown[];
    input: string;
  }
  | {
    // Report in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
    type: "error";
    input: string;
    path: string;
    title: string;
    message: string;
  };

export async function loadCompiler(fs: Fd): Promise<ReplCompiler> {
  // TODO load these two with esbuild? Maybe build to a `generated` dir?
  // Or create a custom esbuild plugin that loads both wasm and js

  const wasm_url = "./repl.wasm";

  const initFailure = async (input: string): Promise<CompileResult> => {
    return {
      type: "error",
      input,
      path: wasm_url,
      title: "Failed to load the WASM compiler",
      message: `Failed to load the WASM compiler at '${wasm_url}'`,
    };
  };

  let __exports: ReplCompiler = {
    evaluate: initFailure,
    check: initFailure,
    removeFromState: async () => null,
  };

  try {
    let fds: Fd[] = [
      new OpenFile(new File([])), // stdin
      ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
      ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
      fs,
    ];
    let wasi = new WASI([], [], fds);
    const { instance } = await WebAssembly.instantiateStreaming(
      fetch(wasm_url),
      {
        ghc_wasm_jsffi: ulmJs(__exports),
        wasi_snapshot_preview1: wasi.wasiImport,
      },
    );
    Object.assign(__exports, instance.exports);
    wasi.initialize(instance);

    const runpack = (fn: string) => {
      return async (input: string): Promise<CompileResult> => {
        try {
          if (typeof instance.exports[fn] !== "function") {
            throw new Error(
              `Wasm module does not export a function called '${fn}'.`,
            );
          } else {
            const strResult = await instance.exports[fn](input);
            return JSON.parse(strResult) as CompileResult;
          }
        } catch (error) {
          const title = `Executing '${fn}(input)' in WASM failed`;
          console.error(title, { input, error });
          return {
            type: "error",
            input,
            path: wasm_url,
            title,
            message: error.message || error.name || JSON.stringify(error),
          };
        }
      };
    };

    __exports.check = runpack("check");
    __exports.evaluate = runpack("evaluate");
    console.info("Successfully loaded the WASM repl compiler");
  } catch (ex) {
    console.error("Loading the WASM repl compiler failed", ex);
  }

  return __exports;
}
