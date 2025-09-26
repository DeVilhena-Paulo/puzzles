# Advent of Code

Solutions to some of the editions of the [Advent of Code](https://adventofcode.com/).

## Running

The solutions are written in [OCaml](https://ocaml.org/). The build system is managed with [`dune`](https://dune.build/). You can get both the OCaml compiler and `dune` with [`opam`](https://opam.ocaml.org/doc/Install.html), the OCaml package manager.

Once you get `opam`, run the following command from the folder `advent-of-code` to create a local opam switch with the correct version of the compiler:

```
$ opam switch create . ocaml-base-compiler.5.3.0
```

Now, install `dune` and other packages used in some of the solutions (`menhir` and `base`):

```
$ opam install dune menhir base
```

To build, run the following command from the directory `advent-of-code`:

```
$ opam build
```

To run a specific solution, say, the solution `2024/day-1/main.ml`, go to `2024/` and run:

```
$ opam exec --no-print-directory ./day-1/main.exe
```
