import git from "isomorphic-git";
import http from "isomorphic-git/http/node";

const isBitbucketUrl = (url: string) => {
  const bitbucketRegexp = /^https:\/\/.+@bitbucket\.org\/.+\/.+\.git$/;
  return !!bitbucketRegexp.exec(url);
};

const getBitbucketAuth = (url: string, token: string): string => {
  const bitbucketUserEndRegexp = /@bitbucket\.org\/.+\/.+\.git$/;
  const match = bitbucketUserEndRegexp.exec(url);
  if (match) {
    const endPosition = match.index;
    const username = url.substring(8, endPosition);
    const rgrgerge = url.replace(
      username,
      token.length === 20 ? `x-token-auth:${token}` : `${username}:${token}`
    );
    return rgrgerge;
  }
  return url;
};

export default class GitFs {
  static async clone(path: string, gitUrl: string, branch?: string, token?: string): Promise<void> {
    try {
      await git.clone({
        /* eslint-disable */
        // @ts-ignore
        fs: window.ligoIdeFileSystemCallback,
        /* eslint-enable */
        http,
        dir: path,
        url: isBitbucketUrl(gitUrl) && token ? getBitbucketAuth(gitUrl, token) : gitUrl,
        corsProxy: process.env.GIT_PROXY,
        ref: branch ? `refs/heads/${branch}` : undefined,
        onAuth: () => {
          if (token && !isBitbucketUrl(gitUrl)) {
            return {
              username: token,
              password: token,
            };
          }
          return { cancel: true };
        },
      });
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
      throw new Error(e.message ? e.message : e);
    }
  }

  static async getRemoteBranches(url: string, token?: string) {
    const refs = await git.listServerRefs({
      http,
      corsProxy: process.env.GIT_PROXY,
      url: isBitbucketUrl(url) && token ? getBitbucketAuth(url, token) : url,
      prefix: "refs/heads/",
      onAuth: () => {
        if (token && !isBitbucketUrl(url)) {
          return {
            username: token,
            password: token,
          };
        }
        return { cancel: true };
      },
    });

    const mainRef = await git.listServerRefs({
      http,
      corsProxy: process.env.GIT_PROXY,
      url: isBitbucketUrl(url) && token ? getBitbucketAuth(url, token) : url,
      prefix: "HEAD",
      symrefs: true,
      onAuth: () => {
        if (token && !isBitbucketUrl(url)) {
          return {
            username: token,
            password: token,
          };
        }
        return { cancel: true };
      },
    });

    return {
      main: mainRef[0].target?.replace("refs/heads/", ""),
      refs: refs.map((r) => r.ref.replace("refs/heads/", "")),
    };
  }
}
