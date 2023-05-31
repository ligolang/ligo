# Fast implementation of hash functions over the scalar field of BLS12-381

**Use with caution before release 1.0.0**

Documentation available [here](https://nomadic-labs.gitlab.io/cryptography/ocaml-bls12-381-hash/bls12-381-hash/).

This library provides a fast implementation of:
- [Poseidon](https://eprint.iacr.org/2019/458)
- [Rescue](https://eprint.iacr.org/2019/426)
- [Anemoi](https://eprint.iacr.org/2022/840)
- [Griffin](https://eprint.iacr.org/2022/403)

## Install

### Use a local switch for development
```
opam switch create ./ 4.14.0
dune build
```

### Install in an existing local switch

```shell
# for the latest published version in ocaml/opam-repository
opam install bls12-381-hash
# for the dev version
opam pin add bls12-381-hash.dev git+https://gitlab.com/dannywillems/ocaml-bls12-381-hash\#main
```

## Run tests

```
dune runtest
```

To get the coverage:
```
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
```

## Run the benchmarks

Install `core_bench`:

```
opam install core_bench
```

See files listed in the directory `benchmark` and execute it with `dune exec`. For instance:
```
dune exec ./benchmark/bench_anemoi.exe
```

## Documentation

```
opam install odoc
dune build @doc
```
