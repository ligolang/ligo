import git from "isomorphic-git";
import http from "isomorphic-git/http/node";

export default class GitFs {
  static async clone(path: string, gitUrl: string): Promise<void> {
    try {
      await git.clone({
        /* eslint-disable */
        // @ts-ignore
        fs: window.ligoIdeFileSystemCallback,
        /* eslint-enable */
        http,
        dir: path,
        url: gitUrl,
        corsProxy: process.env.GIT_PROXY,
      });
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
      throw new Error(e.message ? e.message : e);
    }
  }
}
