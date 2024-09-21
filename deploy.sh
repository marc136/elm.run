#!/bin/sh

npx elm-watch make --optimize
rsync -azP --delete www/dist/ hetzner-nbg1:/www/elm.run/
