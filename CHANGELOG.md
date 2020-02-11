# Changelog

## [Unreleased]

## [9164206ef1fcf3e577820442b5afbad92d03ffa4] - 2020-02-09
### Changed
- Mutation of variables inside lambdas passed to list_iter do not have effect anymore. Side-effects used to survive iterations of list_iter via a quirk in the Michelson list_iter. Now, either use a list_fold and explicitly pass through the updated variables (e.g. storage) to the next iteration, or use a `for` loop which automatically detects mutations within the loop body and lifts the affected variables to a record that is passed from iteration to iteration.

## [Add crypto reference page to docs](https://gitlab.com/ligolang/ligo/-/merge_requests/370)
### Changed
- Corrected typo in CameLIGO/ReasonLIGO front end where Crypto.blake2b was 'Crypto.black2b'
	
## [Failwith do not fail](https://gitlab.com/ligolang/ligo/merge_requests/337) - 2020-01-17
### Added
- running failing code in `ligo interpret`, `ligo dry-run`, `ligo run-function` will no longer be an error (return value : 0)
	
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
