## Motivation and Context
<!--- Why is this change required? What problem does it solve? -->
<!--- If it fixes an open issue, please link to the issue in `Related issues` section -->

### Related issues

Resolves <!-- insert the related issue here, or none if not appliable -->.

### :white_check_mark: Checklist for the LIGO Debugger

- Tests
  - [ ] Bug fixes and new features have corresponding tests added

- Documentation
  - I checked whether I should update the docs and did so if necessary:
    - [ ] README files ([root](/tools/debugger/README.md), [adapter](/tools/debugger/ligo-debugger/README.md), and [plugin](/tools/debugger/vscode-plugin/README.md))
    - [ ] Haddock & plugin's code documentation
    - [ ] [Other documents](/tools/debugger/docs/)

- `launch.json` config (if updated)
  - [ ] I keep the format of `launch.json` (in [package.json](/tools/debugger/vscode-plugin/package.json)) synced with how the plugin code and the adapter's code work with this configuration.
  - [ ] If I changed values in the initial configuration for `launch.json`, I also searched for them in code and updated accordingly. I keep the default configurations and default values for not listed fields in match.

- Breaking changes (if applied)
  - [ ] If I introduced changes that are not compatible with the older versions of `ligo`, I updated our version restrictions according to [the respective document](/tools/debugger/docs/ligo-versions.md).

## Description

<!--- Describe your changes in detail -->

## Component

* [ ] compiler
* [ ] website
* [ ] webide
* [ ] vscode-plugin
* [X] debugger

## Types of changes

* [ ] Bug fix (non-breaking change which fixes an issue)
* [ ] New feature (non-breaking change which adds functionality)
* [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
* [ ] Performance improvement (non-breaking change that improves performance)
* [ ] None (change with no changelog)

## Changelog
<!--- Section under ## Changelog will be added to your changelog description. -->

## Checklist:

* [ ] Changes follow the existing coding style (use `dune @fmt` to check).
* [ ] Tests for the changes have been added (for bug fixes / feature).
* [ ] Documentation has been updated.
* [ ] Changelog description has been added (if appropriate).
* [ ] Start titles under `## Changelog` section with #### (if appropriate).
* [ ] There is no image or uploaded file in changelog
* [ ] Examples in changed behaviour have been added to the changelog (for breaking change / feature).

/label ~"direction::Debugger"
