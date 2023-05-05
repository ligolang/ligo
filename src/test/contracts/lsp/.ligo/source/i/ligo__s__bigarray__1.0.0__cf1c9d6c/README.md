# Bigarray

`Bigarray` is an ordered data structure.

`Bigarray` are one dimensional arrays, they're represented as a list internally.

Like lists, `Bigarray` items must have the same type.

## Usage

This library is aimed to be used as module and can be consumed as a package from LIGO registry.
While we recommend you consult the [documentation](https://ligolang.org/docs/advanced/package-management/), here's a quick tutorial.


1. `ligo install`
Create an empty `package.json` (just `{}`), and run,

```sh
ligo install @ligo/bigarray
```

to install the package.

2. Import the package modules with relative paths. Example: `# import "bigarray-cameligo/lib/bigarray.mligo`


```cameligo
#import "bigarray-cameligo/lib/bigarray.mligo" "Bigarray"

let test_fill =
  begin
    assert (Bigarray.fill 4 10 = [10;10;10;10]);
    assert (Bigarray.fill 4 "foo" = ["foo"; "foo"; "foo"; "foo"])
  end
```

3. Compile with `ligo compile` or `ligo run test`. For the above example, `ligo run test ./that-file.mligo`.

## API Reference

### `.fill`
`Bigarray.fill n initial_value` creates a list of `n` values initialised to `initial_value`

### `.last_exn`
`Bigarray.last_exn a_bigarray` finds the last element in `a_bigarray`. Throws an exception if an empty `Bigarray` was provided.

### `.reverse`
`Bigarray.reverse a_bigarray`
Reverses `a_bigarray`

### `.concat`
`Bigarray.concat a1 a2` concatenates two bigarrays, `a1` and `a2`.

### `.get_exn`
`Bigarray.get_exn a n` gets nth element (starting at 0). Throws an exception if it's an empty bigarray.

### `.set_exn`
`Bigarray.set_env xs n x` sets `x` at the `n`th place in bigarray `xs`. Throws, if `n` is not within `bigarray`.

### `.insert_exn`
`Bigarray.insert_exn xs n x` inserts `x` in the `n`th position in bigarray `xs`. Throws, if index `n` is not within `bigarray`.

### `.remove_exn`
`Bigarray.remove_exn xs n` removes `n`th element from bigarray, `xs`.

### `.drop_exn`
`Bigarray.drop_exn xs n` drops the first `n` elements in bigarray `xs`. Throws if `n` is outside bounds of bigarray,`xs`.

### `.take`
`Bigarray.take xs n` returns first `n`elements of bigarray, `xs`.

### `.slice`
`Bigarray.slice xs start no_elements` returns `no_elements` starting from `start` index in bigarray `xs`

### `.split`
`Bigarray.split xs n` returns a tuple of two bigarray, split at `n`th index.

### `.rotate`
`Bigarray.rotate xs n` rotates a bigarray, `xs`, `n` times. A rotation can be defined as taking the first element of the bigarray and appending it at the end.

## Development

Just run `make` or `make test` to run the tests

You can also override `make` parameters by running :
```sh
make test ligo_compiler=<LIGO_EXECUTABLE> protocol_opt="--protocol <PROTOCOL>"
```
