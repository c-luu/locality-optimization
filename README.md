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

## TODO
1. Front-end compiler.
2. CFG builder.
3. Organize types.
4. Refactor mutable variables.

## Resources
1. [Lecture Slides](https://courses.cs.washington.edu/courses/csep501/18sp/lectures/S-valuenumbering_ink.pdf).
