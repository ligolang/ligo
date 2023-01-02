# Ligo Web IDE

## Deployment

Web IDE is available at https://ligo-webide.serokell.team/.

### Manually

Prerequisites: `x86_64-linux` machine with Nix.

To deploy a new version from CLI you'll need to run:
```
nix develop -c deploy .#webide --ssh-user <user> --skip-checks --remote-build
```
Note that in order to do the deployment, your user needs to have root access to the server.

### From CI

Web IDE deployment can be triggered from the CI manually on each commit to the `tooling` branch.
