# LIGO CLI Completion
The provided script should be compatible with `bash` and `zsh`.

## Linux

### Bash
Make sure that bash completion is installed.

To install completions just put that script in `/etc/bash_completion.d/` or `/usr/share/bash-completion/completions/`. E.g.
```bash
$ sudo cp ligo_autocomplete /usr/share/bash-completion/completions/ligo
```

After that completions should be available.
### Zsh
Installation is the same as for `bash` but you need to add these lines into your `~/.zshrc`
```bash
autoload bashcompinit
bashcompinit
source /usr/share/bash-completion/completions/ligo
```

## macOS
Install `bash-completion` if you don't have it already.
```bash
$ brew install bash-completion
```

Put `ligo_autocomplete` script into `~/.config/bash_completion/completions/ligo`
```bash
$ cp ligo_autocomplete ~/.config/bash_completion/completions/ligo/ligo_autocomplete
```
