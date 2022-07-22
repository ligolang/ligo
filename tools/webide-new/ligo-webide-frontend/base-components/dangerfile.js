import { fail, warn, message, markdown, danger } from "danger"

// Check if it neeed to update yarn.lock
const packageChanged = danger.git.modified_files.includes('package.json');
const lockfileChanged = danger.git.modified_files.includes('package-lock.json');

if (packageChanged && !lockfileChanged) {
	warn("Changes were made to package.json, but not to package-lock.json - <i>'Perhaps you need to run `npm install`?'</i>");
}

// No Big pull request
const bigPRThreshold = 600;
if (danger.github.pr.additions + danger.github.pr.deletions > bigPRThreshold) {
	warn('Big pull request, please keep small to make it easier to review');
}

// Only run this check on release and hotfix branches
const { head, base } = danger.github.pr;
const isReleaseBranch = head.ref.includes('release');
const isHotfixBranch = head.ref.includes('hotfix');
const isMasterBranch = base.ref === 'master';

if (isMasterBranch && !isHotfixBranch && !isReleaseBranch) {
	fail(`Target Branch is not a hotfix or release branch!`);
}

// Check the standard of the commit message
const regex = /^(Merge)|(feat|fix|major|chore|docs|build|hotfix|style|perf|refactor|)([\(（^].*[\)）])?:/g
danger.git.commits.forEach(commit => {
	if (!commit.message.match(regex)) {
		fail(`Commit message '${commit.message}' does match the correct format!`);
	}
})

// Check the standard of the PR title
const { title } = danger.github.pr
if (!title.match(regex)) {
	fail(`The Pull Reqeust title does match the correct format!`);
}
