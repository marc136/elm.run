These are the default packages that elm-init installs
```
direct:
"elm/browser": "1.0.2"
"elm/core": "1.0.5"
"elm/html": "1.0.0"
indirect:
"elm/json": "1.1.3"
"elm/time": "1.0.0"
"elm/url": "1.0.0"
"elm/virtual-dom": "1.0.3"
```

And I generated such tarballs:

1. The default packages that elm-init installs (only source files) `default-packages.tar.gz` is 193.3KiB
2. If I also include http, random and url `default-with-http-random-url.tar.gz` grows to 230.3KiB
3. When I include both sources and built `artifacts.dat` and `docs.json` files, `with-sources-and-artifacts.tar.gz` takes 403.3KiB
4. If I remove the Elm and js source files from that, it is only 256.1KiB
5. And if I remove http and random (and their deps) from that, `artifacts-and-docs.without-http-random.tar.gz` shrinks to 214.7KiB (same packages as in 1)
