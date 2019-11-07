---
id: documentation-and-releases
title: Documentation and releases
---


## Documentation

If you'd like to contribute to the docs you can find them at [`gitlab-pages/docs`]() in raw markdown form.
Deployment of the docs/website for LIGO is taken care of within the CI, from `dev` and `master` branches.

## Releases & versioning

### Development releases (next)

Development releases of Ligo are tagged `next` and are built with each commit to the `dev` branch. Both the docker image & the website are published automatically.

### Stable releases

Releases tagged with version numbers `x.x.x` are built manually, both docs & the docker image. Deployment of the website is handled by the CI.
