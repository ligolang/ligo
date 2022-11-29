# Various LIGO versions support

Throughout the project (Serokell tools for LIGO) we strive to always have support for the most recent version of `ligo` executable.

However, in case of debugger, I (@Martoon) think that we temporarly need stricter rules as interaction with `ligo` is yet not mature and it introduces breaking changes too often.

## Version compatibility constraints

When debugger functionality is demanded by the user, we check for the `ligo` version.

Keeping things simple, we assume that with `ligo` versions below a minimally supported version we are not compatible and debugger start fails for them.
For the newest versions of `ligo` that are not accounted by our code, we appriory assume that everything is fine; but in case of failure, we make the user know that he is using a potentially incompatible version.

## Minor tests

We can mark some tests as `minor` ones. These tests can fail when the used `ligo` version is only partially supported by us. We can turn them off by passing a `-m` flag to test arguments:
```bash
$ stack test --ta "-m"
```

## Updating version constraints

Constraints on `ligo` version are specified by `isSupportedVersion` function, we have to keep them up-to-date.

For that, let's follow the following rules.

### Upper bound

When anybody notices that a new version of `ligo` is released, he is free to create a commit that updates the upper bound.
This commit can be created in a new MR or in any existing MR just by the way, as upper version bump should happen often and we want to have this process simple.
Running tests is a sufficient proof of that the new version can be treated as supported.

Also, we should periodically check when a new verion of `ligo` is released, e.g. at [this page](https://gitlab.com/ligolang/ligo/-/releases).

### Lower bound

When we update our code in a way that is not compatible with older versions of `ligo`, in particular when `ligo` itself introduces permanent non-backward compatible changes that we follow up, then we have to bump the lower bound version.

In such case, other constraints should be updated accordingly, e.g. it may be that all exclusion rules become redundant.

### Excluded versions

When `ligo` gains a bug that fully or partially breaks compatibilty and we expect this to be resolved without changes on our side, add an exclusion rule for that particular version or a range of versions.
