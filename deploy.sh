#!/bin/sh

make all
rsync -azP --delete www/dist/ hetzner-nbg1:/www/elm.run/
