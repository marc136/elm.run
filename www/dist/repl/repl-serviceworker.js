const VERSION = "v2";
const OFFLINE_CACHE = `repl-${VERSION}`;

const FILES_TO_CACHE = [
  "/repl/",
  "/repl/index.html",
  "/editor/codemirror.css",
  "/repl/ulm-repl.css",
  "/editor/codemirror.js",
  "/repl/ulm-repl.js",
  "/repl/ulm-repl.mjs",
  // TODO generate these entries in build script
  // "/repl/repl-wasm-TOQZCPM6.mjs", // online
  "/repl/repl-wasm-GDAG2FJV.mjs", // local
  "/repl/repl.wasm",
  "/elm-all-examples-package-artifacts.tar.gz",
  // favicon
  "/favicon.ico",
  // webmanifest
  "/repl/manifest.json",
  // from manifest.json
  "/android-chrome-192x192.png",
  "/android-chrome-512x512.png",
];

self.addEventListener("fetch", async (event) => {
  console.warn("DEV: fetch call", event.request);
  event.respondWith(fetchResponse(event.request));
});

self.addEventListener("install", (event) => {
  console.warn("installing service worker", VERSION);
  event.waitUntil(populateOfflineCache());
  console.warn("all files cached");
});

self.addEventListener("activate", (event) => {
  event.waitUntil(replaceOlderServiceWorker());
});

/** @param request {Request} */
async function fetchResponse(request) {
  const cache = await caches.open(OFFLINE_CACHE);
  const cached = await caches.match(request);
  if (cached) return cached;

  const response = await fetch(request);
  if (response.ok) {
    cache.put(request, response.clone());
  }
  return response;
}

async function populateOfflineCache() {
  const cache = await caches.open(OFFLINE_CACHE);
  cache.addAll(FILES_TO_CACHE);
}

async function replaceOlderServiceWorker() {
  const names = await caches.keys();
  await Promise.all(
    names.map((name) => {
      if (name !== OFFLINE_CACHE) {
        return caches.delete(name);
      }
    }),
  );
  await clients.claim();
}
