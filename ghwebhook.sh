#!/bin/bash

set -ex

su - mpidl-web -c '
set -ex

cd mpidl-web/
git checkout master
git pull
git submodule update -i

cd msgpack-haskell/msgpack-idl
cabal-dev install --reinstall -s ../../site/msgpack-idl
cd ../..

cd site
rm -r dist/
cabal-dev install
'
stop mpidl-web || true
sleep 1
start mpidl-web
