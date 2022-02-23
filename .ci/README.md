# CI Documentation
## Release doc automation flow
### Why
Each 2 week, a release is created. Some actions have to be done :
- Create a new version of documentation which is used to generate the ligolang website.
- Remove the oldest version, we want to manage 3 version + next.
- Update the references about how to install ligo in thoses versions
- Update the web-ide Image to 

This release creation can have some risk like forgot to update version used by web-ide


### How 
#### User action
To trigger the process :
- Create a branch named release/$VERSION (replace $VERSION with the version to create)
- Push the branch
- Wait
- Check generated MR and merge it into dev if you agree

#### Process flow
- Human create the branch named “release/X.Y.Z”
- Pipeline process 1 : Create and push tag X.Y.Z trigger process 2
- Pipeline process 2 (actual flow triggered by tag push) : create release
- Pipeline process 2 : edit metadata file into branch release/X.Y.Z trigger process 3
- Pipeline process 3 : Docusaurus command to generate new version
- Pipeline process 3 : Remove oldest version
- Pipeline process 3 : edit installation doc (update binaries deb link, docker version) and web-ide dockerfile to target freshly created ligo-version
- Pipeline process 3 : create MR release/X.Y.Z to dev
- Human : Ask for merging (if we are sure of the process, maybe we can check to autorize merge without human action) it'll trigger process 4
- Pipeline process 4 : deploy web-ide and update gitlab-pages (this second part is independant of gitlab ci)

## Amelioration
- Secure token used for push
- Use docker image with tools already installed and git already configured
- Use Infra account of ligo instead of personal account.
- Avoid human action when process will be sure (merge automatically, trigger automatically)
---
