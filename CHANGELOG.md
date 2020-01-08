# Changelog

## [Unreleased]

## [1899dfe8d7285580b3aa30fab933ed589f8f1bc5] - 2020-01-08
### Added
- Partial application and OCaml-like currying behavior to CameLIGO & ReasonLIGO

### Changed
- Contract entrypoints now need to use tuple parameters in CameLIGO

### Docs
- Explain currying
- Now use tuple parameters in function examples
- Now have map examples which are runnable as a combined block for doc tests

### Tests
- More tests take advantage of tuple parameter destructuring (i.e.
`let thing (p,s: param * storage) : int = ...`)
- Entrypoints now use tuple parameters
- Added Currying behavior test


## [Changelog Patch](https://gitlab.com/ligolang/ligo/merge_requests/300) - 2020-01-08
### Added
- CHANGELOG.md that keeps track of notable changes to LIGO, etc
- 'changelog' command to LIGO command line that dumps CHANGELOG.md to stdout
- Help tips message with options for getting assistance that's printed on error
