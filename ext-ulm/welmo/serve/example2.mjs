import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory, Directory } from "./node_modules/@bjorn3/browser_wasi_shim/dist/index.js";

function strToFile(str) {
    return new File(new TextEncoder("utf-8").encode(str))
}

const pkgDir = new Directory([])
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
            "elm/html": "1.0.0"
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
    const wasm = await loadWasm('ulm')
    const packages = [
        "elm/core/1.0.5",
        "elm/html/1.0.0",
        "elm/browser/1.0.2",
        // indirect dependencies
        "elm/json/1.1.3",
        "elm/virtual-dom/1.0.3",
    ];
    for (const pkg of packages) {
        await loadArtifacts(pkg);
    }

    async function compileHtml(source) {
        const raw = await wasm.compile(source)
        const result = JSON.parse(raw);
        console.info('Compilation result:', result);

        if (result.type === 'success' && result.file && result.name) {
            const file = fs.dir.contents.get('tmp').contents.get(result.file);
            return wrapBufferInHtml(file.data, result.name);
        } else {
            throw result
        }
    }

    return { wasm, compileHtml, fs, pkgDir };
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


/**
 * @param {string} name 
 */
async function loadWasm(name) {
    const wasm_url = `./${name}.wasm`;
    const js_url = `./${name}.js`;

    let __exports = {};
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
}

async function loadArtifacts(pkg) {
    const fullpath = `${packagePath(pkg)}/artifacts.dat`;
    const response = await fetch(`./${fullpath}`);
    const raw = await response.arrayBuffer();
    const paths = pkg.split("/");
    // note the `dir` field only exists for `PreopenedDirectory`
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
}

function writeFile(dir, name, arrayBuffer) {
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

export function wrapBufferInHtml(arrayBuffer, name) {
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