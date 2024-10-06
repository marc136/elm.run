import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import type { Fd } from "@bjorn3/browser_wasi_shim";
import type { AsCustomEvent } from './ts-interop-helpers'

// TODO extract this into the elm-compiler-wasm repo

// import ulmWasm from '../../www/generated/ulm.wasm' 
import ulmJs from '../../www/generated/ulm'

export interface Compiler {
    compile: (source: string) => Promise<CompileResult>
    make: (filepath: string) => Promise<CompileResult>
}

// // CompilerReport in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
// // Report in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
// export type CompileResult = AsCustomEvent<'compile-result'>
export type CompileResult =
    {
        type: 'success',
        // full file path of generated js code
        file: string,
        // Elm module name
        name: string
    }
    | // CompilerReport in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
    {
        type: 'compile-errors'
        errors: ElmCompilerError[]
    }
    | // Report in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
    {
        type: 'error',
        path: string
        title: string
        message: string
    }

type ElmCompilerError = unknown

export async function loadCompiler(fs: Fd): Promise<Compiler> {
    // TODO load these two with esbuild? Maybe build to a `generated` dir?
    // Or create a custom esbuild plugin that loads both wasm and js

    const wasm_url = './ulm.wasm';

    const initFailure = async (input: string): Promise<CompileResult> => {
        return {
            type: "error",
            path: wasm_url,
            title: "Failed to load the WASM compiler",
            message: `Failed to load the WASM compiler at '${wasm_url}'`,
        };
    };

    let __exports: Compiler = {
        compile: initFailure,
        make: initFailure,
    };

    try {
        let fds: Fd[] = [
            new OpenFile(new File([])), // stdin
            ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
            ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),
            fs,
        ];
        let wasi = new WASI([], [], fds);
        const { instance } = await WebAssembly.instantiateStreaming(
            fetch(wasm_url),
            {
                ghc_wasm_jsffi: ulmJs(__exports),
                wasi_snapshot_preview1: wasi.wasiImport,
            }
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
                        path: wasm_url,
                        title,
                        message: error.message || error.name || JSON.stringify(error),
                    };
                }
            };
        };

        __exports.compile = runpack("compile");
        __exports.make = runpack("make");

        console.info("Successfully loaded the WASM compiler");
    } catch (ex) {
        console.error("Loading the WASM repl compiler failed", ex);
    }

    return __exports;
}
