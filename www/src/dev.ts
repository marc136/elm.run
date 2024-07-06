import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory, Directory } from "@bjorn3/browser_wasi_shim";
import * as zip from "@zip.js/zip.js";


function strToFile(str) {
    return new File(new TextEncoder("utf-8").encode(str))
}

const pkgDir = new Directory([])
// @ts-expect-error I don't pass in a map of `INode`
const fs = new PreopenDirectory("/", [
    ["elm.json", strToFile(`{
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
            "elm-community/maybe-extra": "5.3.0"
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
`
    )],
    ["src", new Directory([
        ["Static.elm", strToFile(`module Static exposing (main)

import Html exposing (Html)

main : Html msg
main =
    Html.text "main"
`
        )],
    ])],
    ["tmp", new Directory([])],
    ["packages", pkgDir],
])
console.log('fs', fs)

export async function init() {
    const wasm = await loadCompiler()
    const packages = [
        "elm/core/1.0.5",
        "elm/html/1.0.0",
        "elm/browser/1.0.2",
        // indirect dependencies
        "elm/json/1.1.3",
        "elm/virtual-dom/1.0.3",
        "elm/time/1.0.0",
        "elm/url/1.0.0",
    ];
    for (const pkg of packages) {
        await loadArtifacts(pkg);
    }

    await loadSources()
    await loadRegistry()

    async function compileHtml(source) {
        const raw = await wasm.compile(source)
        const result = JSON.parse(raw);
        console.info('Compilation result:', result);

        if (result.type === 'success' && result.file && result.name) {
            const data = readFile(result.file)
            return wrapBufferInHtml(data, result.name);
        } else {
            throw result
        }
    }

    return { wasm, compileHtml, fs, pkgDir };
}

async function loadSources() {
    const pkgs = '/elm-home/0.19.1/packages';
    const maybe = "elm-community/maybe-extra/5.3.0";
    const elmJson = await (await fetch(`${pkgs}/${maybe}/elm.json`)).arrayBuffer();
    const src = await (await fetch(`${pkgs}/${maybe}/src/Maybe/Extra.elm`)).arrayBuffer();
    const dir = new Directory([
        ["maybe-extra", new Directory([
            ["5.3.0", new Directory([
                ["elm.json", new File(elmJson)],
                ["src", new Directory([
                    ["Maybe", new Directory([
                        ["Extra.elm", new File(src)]
                    ])]
                ])]
            ])]
        ])]
    ]);
    pkgDir.contents.set("elm-community", dir);
}

async function loadRegistry() {
    // TODO query https://package.elm-lang.org/all-packages
    // or query https://package.elm-lang.org/all-packages/since/{package-count}
    // then send it to wasm and store it in binary form
    // -> See `fetch` in elm-compiler-wasm/builder/src/Deps/Registry.hs
    const allpackages = await fetch('/elm-home/0.19.1/packages/registry.dat')
    pkgDir.contents.set('registry.dat', new File(await allpackages.arrayBuffer()))
}

/**
 * @param {string} filepath
 * @returns {Uint8Array} file content
 */
export function readFile(filepath) {
    let node = fs.dir
    const paths = filepath.split('/');
    for (const p of paths) {
        if (p.trim() !== '') {
            node = node.contents.get(p)
        }
    }
    return node.data
}

export function printFs() {
    const withIndent = (indent, node) => {
        let str = node.constructor.name ?? '<unknown>'
        if (node.contents) {
            str += ` [${node.contents.size}]\n`
            node.contents.forEach((val, key) => {
                str += '  '.repeat(indent) + key + ' ' + withIndent(indent + 1, val)
            })
        } else if (node instanceof File) {
            str += ` (${node.size} byte)\n`
        } else {
            str += '\n'
        }
        return str
    }

    // start with a `PreopenDirectory`
    //  -> has a `prestat_name: string` and a `dir: Directory`
    console.log(fs.prestat_name, withIndent(1, fs.dir))
}

interface Compiler {
    compile: (code: string) => CompileResult
}

type CompileResult =
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

async function loadCompiler(): Promise<Compiler> {
    const name = 'ulm'
    const wasm_url = `./${name}.wasm`;
    const js_url = `./${name}.js`;

    let __exports = {} as Compiler;
    let fds = [
        new OpenFile(new File([])), // stdin
        ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
        ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),
        fs,
        // pkgDir,
    ];
    let wasi = new WASI([], [], fds);
    const { instance } = await WebAssembly.instantiateStreaming(
        fetch(wasm_url),
        {
            ghc_wasm_jsffi: (await import(js_url)).default(__exports),
            wasi_snapshot_preview1: wasi.wasiImport,
        }
    );
    Object.assign(__exports, instance.exports);
    wasi.initialize(instance);

    return __exports;
}

export async function installPkg(name, version) {
    console.error('TODO')
    const pkg = name + '/' + version;
    const fullpath = packagePath(pkg) + 'src/Result/Extra.elm';
    const response = await fetch(`./${fullpath}`)
    const raw = await response.arrayBuffer();

    const paths = fullpath.split('/');
    let dir = pkgDir.dir;
    for (const name of paths) {
        console.log(name, { dir })
        if (name === 'Extra.elm') {
            console.warn('TODO write Extra.elm file');
            break
        }
        console.log('dir', { dir, name })
        const entry = dir?.contents.get(name);
        if (entry) {
            dir = entry.dir ?? entry
        } else {
            let { ret, entry } = dir.create_entry_for_path(name, true);
            if (ret !== 0) throw new Error(`Could not create directory '${name}' (ERROR: ${ret})`);
            dir = entry;
        }
    }
    writeFile(dir, 'Extra.elm', raw);
}

function packagePath(pkg: string) {
    return `/elm-home/0.19.1/packages/${pkg}`;
}

async function loadArtifacts(pkg: string) {
    // const lastIndex = pkg.lastIndexOf('/')
    // const pkgName = pkg.substring(0, lastIndex)
    // const pkgVersion = pkg.substring(lastIndex + 1)
    // // const remote = `https://github.com/${pkgName}/zipball/${pkgVersion}/`
    // const remote = "https://github.com/elm/browser/archive/refs/tags/1.0.2.zip"
    // console.info(pkg, 'loading source code and artifacts for', { pkgName, pkgVersion, remote })
    // // const zipball = await fetch(remote)
    // const r = new zip.HttpReader(remote, {useXHR:true})
    // // await r.init()
    // const zipReader = new zip.ZipReader(r)
    // console.warn(await zipReader.getEntries())
    // debugger

    // Downloading the zipballs in the browser is a problem, because they have no CORS headers set
    // I will either need to proxy over a server or come up with a different idea
    // for now it is sufficient to 
    // 1. download the elm.json file 
    // 2. load the precomputed `artifacts.dat`
    // 3. create an empty directory `src`
    const elmJson = await fetch(`https://raw.githubusercontent.com/${pkg}/elm.json`)

    const fullpath = `${packagePath(pkg)}/artifacts.dat`;
    const response = await fetch(`./${fullpath}`);
    const raw = await response.arrayBuffer();
    const paths = pkg.split("/");
    // @ts-expect-error the `dir` field only exists for `PreopenedDirectory`
    let dir = pkgDir.dir ?? pkgDir;
    // debugger
    for (const name of paths) {
        console.log(name, { dir })
        const entry = dir?.contents.get(name);
        if (entry) {
            dir = entry.dir ?? entry
        } else {
            let { ret, entry } = dir.create_entry_for_path(name, true);
            if (ret !== 0) throw new Error(`Could not create directory '${name}' (ERROR: ${ret})`);
            dir = entry;
        }
    }

    writeFile(dir, 'artifacts.dat', raw);
    writeFile(dir, 'elm.json', await elmJson.arrayBuffer())
    dir.contents.set('src', new Directory([]))
}

function writeFile(dir: Directory, name: string, arrayBuffer: ArrayBuffer) {
    // let result = dir.create_entry_for_path(name);
    // if (result.ret !== 0) throw new Error(`Could not create file '${name}' for package '${pkg}' (ERROR: ${result.ret})`);
    // result = result.entry.path_open(0, 0n, 0);
    // if (result.ret !== 0) throw new Error(`Could not open file '${name}' for package '${pkg}' (ERROR: ${result.ret})`);
    // const fd = result.fd_obj;
    // result = fd.fd_allocate(0, arrayBuffer.length);
    // console.log('allocated', result);
    // result = fd.fd_pwrite(arrayBuffer, 0n);
    // console.log('wrote', result, arrayBuffer)
    // if (result.ret !== 0) throw new Error(`Could not write to file '${name}' for package '${pkg}' (ERROR: ${result.ret})`);
    // fd.fd_close();
    dir.contents.set(name, new File(arrayBuffer));
}

export function wrapBufferInHtml(arrayBuffer: ArrayBuffer, name: string): string {
    const code = new TextDecoder().decode(arrayBuffer);
    return wrapInHtml(code, name);
}

export function wrapInHtml(code, name) {
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
</html>`
}
