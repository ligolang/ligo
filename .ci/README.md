# CI Documentation
## Generate changelog
### Why:
When developer create an MR, we want to add a changelog entry which is generated from informations retrieve from MR
- <b><ins>TITLE:</ins></b>  Retrieve from the MR name
- <b><ins>DESCRIPTION:</ins></b> (optional)  Retrieve from the MR description
- <b><ins>TYPE:</ins></b>  Retrieve from the MR description after "type:"
- <b><ins>MERGE_REQUEST:</ins></b>  The merge request IID, used to generate link to the MR, used also as unique name for the file which represent changelog entry. Under line "Changelog details:". <b>Markdown format is preserved</b> 
- <b><ins>AUTHOR:</ins></b>  The author of the MR

Entry is used to generate changelog and release-note.

### How:
If there is no changelog entry corresponding to your MR IID, in stage post-build-action, your pipeline will be stopped you can launch the job, it'll generate a new changelog entry and push it into your branch. 
Because the push gonna trigger a new pipeline, this one gonna be aborted.

So you can push whatever you want, you pipeline will be tested. When you decide to generate your changelog, activate the manual job.

### Edit changelog manually:
Create a new file with the IID of your MR under changelog folder by using the bash script add-changelog-entry.sh.
```bash
usage() {
    printf 'Usage: %s <TYPE> <MERGE_REQUEST> <AUTHOR> <TITLE>\n' "$0"
    printf 'Usage: %s <TYPE> <MERGE_REQUEST> <AUTHOR> <TITLE> <DESCRIPTION>\n' "$0"
    echo "where"
    echo "  TYPE is one of breaking, added, fixed, changed, deprecated, removed, performance, internal, other"
    echo "  MERGE_REQUEST is a merge request ID like 123"
    echo "  AUTHOR is your name"
    echo "  TITLE is a concise one-line description of your changes."
    echo "  DESCRIPTION<optional> is a multi-line markdown formatted"
}
```
Description is optional and yaml formatted (\n as line breaker). Prefer to use automatisation in MR if you want to add a description

### Create an MR without changelog:
In your MR description use type:None. No changelog entry is needed to be merged.

### Recreate the changelog:
You only need to remove the generated file under changelog folder by revert the bot commit (or your commit if you generate it manually).

### Changelog protection:
If you skip the job which generate the changelog and activate the merge train, it'll do the action then fail, you have to relaunch the merge train.
For example if your type is None then you decide to edit it with something else (after build has been done). This will ensure to execute the changelog entry generation.

---

## Release doc automation flow
### Why:
Each 2 week, a release is created. Some actions have to be done :
- Create a new version of documentation which is used to generate the ligolang website.
- Remove the oldest version, we want to manage 3 version + next.
- Update the references about how to install ligo in thoses versions
- Update the web-ide Image to 

This release creation can have some risk like forgot to update version used by web-ide


### How :
#### User action:
To trigger the process :
- Create a branch named release/$VERSION (replace $VERSION with the version to create)
- Push the branch
- Wait
- Check generated MR and merge it into dev if you agree

#### Process flow:
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

## Amelioration:
- Secure token used for push
- Use docker image with tools already installed and git already configured
- Use Infra account of ligo instead of personal account.
- Avoid human action when process will be sure (merge automatically, trigger automatically)
---
