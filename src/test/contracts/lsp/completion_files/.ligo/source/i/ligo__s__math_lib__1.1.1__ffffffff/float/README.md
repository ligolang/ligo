# math-lib-float

This library provides `Float` & `TrigoFloat` modules which export the following funcitons.

`Float` module
1. `new`      - `int -> int -> Float.t`
2. `inverse`  - `Float.t -> Float.t`
3. `add`      - `Float.t -> Float.t -> Float.t`
4. `sub`      - `Float.t -> Float.t -> Float.t`
5. `lt`       - `Float.t -> Float.t -> bool`
6. `lte`      - `Float.t -> Float.t -> bool`
7. `gte`      - `Float.t -> Float.t -> bool`
8. `gt`       - `Float.t -> Float.t -> bool`
9. `mul`      - `Float.t -> Float.t -> Float.t`
10. `div`     - `Float.t -> Float.t -> Float.t`
11. `modulo`  - `Float.t -> Float.t -> Float.t`
12. `resolve` - `Float.t -> nat -> int`

`TrigoFloat` module
1. `sinus`   - `Float.t * nat -> Float.t`
2. `cosinus` - `Float.t * nat -> Float.t`

The floating poing numbers are represented by a pair (a,b) where n = a * 10^b. The scientifc notation considers that the `a` is defined as a float between -10 and 10) and `b` is a `nat`. Since there is no native float type, the `a` is a `nat` and can be greater than 10. All operations are applied in order to keep the number as a pair (a,b) where n = a * 10^b.

Basde on floating-point representation, this library introduces an implementation of trigonometric functions (cosinus, sinus). The implementation of sinus is based on Chebychev polynoms interpolation.

### Tests

A makefile is provided to launch tests.
```
make test
```