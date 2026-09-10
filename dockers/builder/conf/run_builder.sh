#!/bin/sh

cd app/
sudo chown -R opam:opam src/
sudo chmod -R a+rwX src/
cd src/
echo "hello world"
opam exec -- dune build -w