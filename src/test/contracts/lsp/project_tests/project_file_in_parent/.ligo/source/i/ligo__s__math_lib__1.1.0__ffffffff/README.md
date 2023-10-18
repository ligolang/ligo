# math-lib-cameligo

This repository contains a math library for [LIGO](https://ligolang.org/). It is
provided in two flavors (`Float` & `Rational`), and the project is structured as 
follows 

1. [core](./core/README.md): This contains basic math functions
   like `isqrt`, `power`, `factorial`, `min`, `max`, `log_10`.
2. [float](./float/README.md): This provides floating point arithmetic
   for LIGO (which is not supported natively by Michelson). It also provides some
   useful trigonometric functions.
3. [rational](./rational/README.md): This provides rational arithmetic
   for LIGO (which is not supported natively by Michelson). It also provides some
   useful trigonometric functions.