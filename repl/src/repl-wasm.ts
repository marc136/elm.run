import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import type { Fd } from "@bjorn3/browser_wasi_shim";

import ulmJs from '../../www/generated/repl'

export interface Compiler {
    compile: (source: string) => Promise<CompileResult>
    make: (filepath: string) => Promise<CompileResult>
    repl: (code: string) => Promise<string>
    removeFromState: (name: string) => Promise<null>
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

    const wasm_url = './repl.wasm';

    let __exports = {} as Compiler;
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

    __exports.compile = async (source: string) => {
        const strResult: string = await instance.exports.compile(source)
        return JSON.parse(strResult) as CompileResult
    }

    return __exports;
}
