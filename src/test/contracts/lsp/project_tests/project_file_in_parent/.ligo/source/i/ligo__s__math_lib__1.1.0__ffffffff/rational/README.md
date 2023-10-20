# math-lib-rational

This library provides `Rational` & `TrigoFloat` modules which export the following funcitons.

`Rational` module
1. `new`      - `int -> int -> Rational.t`
2. `inverse`  - `Rational.t -> Rational.t`
3. `add`      - `Rational.t -> Rational.t -> Rational.t`
4. `sub`      - `Rational.t -> Rational.t -> Rational.t`
5. `lt`       - `Rational.t -> Rational.t -> bool`
6. `lte`      - `Rational.t -> Rational.t -> bool`
7. `gte`      - `Rational.t -> Rational.t -> bool`
8. `gt`       - `Rational.t -> Rational.t -> bool`
9. `mul`      - `Rational.t -> Rational.t -> Rational.t`
10. `div`     - `Rational.t -> Rational.t -> Rational.t`
11. `modulo`  - `Rational.t -> Rational.t -> Rational.t`
12. `resolve` - `Rational.t -> nat -> int`

`TrigoRational` module
1. `sinus`   - `Rational.t * nat -> Rational.t`
2. `cosinus` - `Rational.t * nat -> Rational.t`


The rational numbers are represented by a pair (p, q) where n = p / q. All operations are applied in order to keep the number as a product and division of rationals.

Based on rational numbers representation, this library introduces an implementation of trigonometric functions (cosinus, sinus). The implementation of sinus is based on Chebychev polynomials interpolation.

### Tests

A makefile is provided to launch tests.
```
make test
```
