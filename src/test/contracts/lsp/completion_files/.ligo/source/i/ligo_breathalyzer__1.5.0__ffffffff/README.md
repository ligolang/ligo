# Breathalyzer test framework

A simple test framework for the Ligo language. The name `Breathalyzer` is a wink
to [Alcotest](https://github.com/mirage/alcotest), a powerful test framework for
the OCaml language.

The best way to understand the design and use of **Breathalyzer** is to consult
the various examples in the [`examples/`](examples/) directory.

## Some examples

The test framework is tested with itself, however, there are several fairly
explicit examples of how to run a test suite:

- [List](examples/simple): A fairly simple demonstration that shows how to make
  simple assertions. A library is declared and some test cases are provided
  which check the behaviour of the functions.
- [Auction](examples/auction): A slightly more complicated test suite that
  demonstrates the notion of **context**, how to _act as an actor_ and how to
  wait (`Expect`) for an error from the transfer execution to a smart-contract.
- [Ticket factory](examples/auction): An example that takes advantage of tickets
  and the exploitation of multiple contracts. A real (partly real-world) example
  that attempts to use all the features of the testing framework.
