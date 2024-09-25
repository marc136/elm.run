DIST = www/dist

.PHONY: all
all: repl interpreter

.PHONY: repl
repl:
	echo build repl
	cd repl && npx elm-watch make --optimize
	node compress-elm-js.mjs $(DIST)/repl/ulm-repl.js

.PHONY: interpreter
interpreter: miniBill-elm-interpreter/index.html
	echo build minibill-elm-interpreter
	# build js
	cd miniBill-elm-interpreter && make optimize
	# copy html file
	cp $^ $(DIST)/minibill-elm-interpreter/
	# copy js, append the git commit shorthash, compress it and use it in html
	DIR=$(DIST)/minibill-elm-interpreter &&\
	COMMIT=$$(git rev-parse --short HEAD:miniBill-elm-interpreter) &&\
	cp miniBill-elm-interpreter/dist/ui.js $$DIR/ui.$$COMMIT.js &&\
	node compress-elm-js.mjs $$DIR/ui.$$COMMIT.js &&\
	sed -i "s|dist/ui.js|ui.$$COMMIT.js|" $$DIR/index.html
