# Readme for maintainers of this project
…on how to use and update the Nix Flakes

### Why Nix?

Cause it can build stuff predictably. I (@cab) as a maintainer don't have to
write 3 separate deployments for our infra, for development and for CI — all of
it works from Nix, cause it can create the same environment everywhere.

### How Nix?

Depends on what you ask. You can get reproducible Nix development shell by
running `nix develop` on a sufficently new and properly configured Nix (TODO
update with the guide on how to do that).

You can also check that everything related builds (both server and frontend) by
running `nix flake check .`

### Stackage
Sometimes you may see something like this:
```
error: This version of stackage.nix does not know about the Stackage resolver lts-19.10.
       You may need to update haskell.nix to one that includes a newer stackage.nix.
```
This happens when you update Stack resolver and Nix Stackage bindings are not yet up to date.
You can usually fix that by running `nix flake update`.
