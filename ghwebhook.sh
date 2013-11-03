#!/bin/bash

set -ex

su - mpidl-web -c '
set -ex

cd mpidl-web/
git checkout master
git pull

cd site
rm -r dist/
cabal-dev install
'
stop mpidl-web || true
sleep 1
start mpidl-web
