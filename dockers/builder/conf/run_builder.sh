#!/bin/sh

cd app/
sudo chown -R opam:opam src/
sudo chmod -R a+rwX src/
cd src/
opam exec -- dune build -w