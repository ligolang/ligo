

### :white_check_mark: Checklist for your Merge Request

#### Related changes (conditional)

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
