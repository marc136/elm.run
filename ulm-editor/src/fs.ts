import { File, Directory, PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import type { Inode } from "@bjorn3/browser_wasi_shim";
import type { TarFileItem } from "nanotar";
// import { parseTarGzip } from "nanotar";

export const pkgDir = new Directory([]);
const srcDir = new Directory([]);
// @ts-expect-error I don't pass in a map of `INode`
export const fs = new PreopenDirectory("/", [
  ["src", srcDir],
  ["tmp", new Directory([])],
  [
    "elm-home",
    new Directory([["0.19.1", new Directory([["packages", pkgDir]])]]),
  ],
  ["packages", new Directory([])], // TODO remove this directory, currently wasm still writes test files to it
]);

const encoder = new TextEncoder();
const decoder = new TextDecoder("utf-8");

export function printFs() {
  const withIndent = (indent, node) => {
    let str = node.constructor.name ?? "<unknown>";
    if (node.contents) {
      str += ` [${node.contents.size}]\n`;
      node.contents.forEach((val, key) => {
        str += "  ".repeat(indent) + key + " " + withIndent(indent + 1, val);
      });
    } else if (node instanceof File) {
      str += ` (${node.size} byte)\n`;
    } else {
      str += "\n";
    }
    return str;
  };

  // start with a `PreopenDirectory`
  //  -> has a `prestat_name: string` and a `dir: Directory`
  console.log(fs.prestat_name, withIndent(1, fs.dir));
}

export function addSourceFile(name: string, content: string) {
  if (name.includes("/"))
    throw new Error("Creating directories is not supported yet");
  writeFileInDir(srcDir, name, content);
}

export function readFileToString(filepath) {
  return readFile(filepath).then((buffer) => decoder.decode(buffer));
}

function readFile(filepath: string): Promise<Uint8Array> {
  let node: Inode = fs.dir;
  const paths = filepath.split("/");
  for (const p of paths) {
    if (p.trim() !== "") {
      if (node instanceof Directory) {
        const next = node.contents.get(p);
        if (next) {
          node = next;
        } else {
          Promise.reject(`Could not find fs entry "${p}" in path ${filepath}`);
        }
      } else {
        console.error("Expected a directory, but got", node);
        Promise.reject(`Expected a directory`);
      }
    }
  }
  if (node instanceof File) {
    return Promise.resolve(node.data);
  } else {
    const msg = `Expected "${filepath}" to be a file`;
    console.error(msg, node);
    return Promise.reject(msg);
  }
}

function createDir(dirpath: string): Directory {
  dirpath = dirpath.replaceAll("//", "/");
  while (dirpath.startsWith("/")) dirpath = dirpath.slice(1);
  while (dirpath.endsWith("/")) dirpath = dirpath.slice(0, -1);
  const paths = dirpath.split("/");
  const name = paths[paths.length - 1];
  let node: Directory = fs.dir;
  // ensure parent directories exist
  for (const segment of paths) {
    let next = node.contents.get(segment) as Directory;
    if (!next) {
      next = new Directory([]);
      node.contents.set(segment, next);
    }
    node = next;
  }
  return node;
}

export async function writeFile(
  filepath: string,
  content: string | ArrayBuffer,
) {
  filepath = filepath.replaceAll("//", "/");
  const lastSlash = filepath.lastIndexOf("/");
  let dir: Directory = fs.dir;
  // no need to create a directory if the path is starting with `/`
  if (lastSlash > 0) {
    dir = createDir(filepath.substring(0, lastSlash));
  }
  await writeFileInDir(dir, filepath.substring(lastSlash + 1), content);
}

export function writeFileInDir(
  dir: Directory,
  name: string,
  content: string | ArrayBuffer,
) {
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
  const buf = typeof content === "string" ? encoder.encode(content) : content;
  dir.contents.set(name, new File(buf));
}

export async function unpackInto({
  dest,
  tar,
  replace,
  replaceValue,
}: {
  dest: string;
  tar: TarFileItem<Uint8Array>[];
  replace: string | undefined;
  replaceValue: string | undefined;
}) {
  for (const file of tar) {
    let fullpath = file.name;
    if (replace && replaceValue !== undefined) {
      fullpath = fullpath.replace(replace, replaceValue);
    }
    while (fullpath.startsWith("/")) fullpath = fullpath.slice(1);
    while (fullpath.endsWith("/")) fullpath = fullpath.slice(0, -1);
    fullpath = (dest! === "/" ? "" : dest) + "/" + fullpath;

    switch (file.type) {
      case "file":
        writeFile(fullpath, file.data ?? new ArrayBuffer(0));
        break;
      case "directory":
        createDir(fullpath);
        break;
      default:
        console.error(
          `Cannot handle file.type='${file.type}' and ignoring it`,
          file,
        );
    }
  }
}
