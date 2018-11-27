# Locality Optimizations
This project explores two compiler techniques for locality optimization at the following levels:
1. Local
2. Regional

## Setup
> docker pull ocaml/opam

> docker run -it --rm -v "$(pwd)":/src ocaml/opam /bin/bash

> opam install utop

## Compile
> ocamlc -o foo foo.ml
