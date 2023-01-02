/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-unsafe-argument */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import Gists from "gists";
import { WebIdeApi } from "~/components/api/api";
import { SourceFile } from "~/components/api/generated";

export type GistContent = { [a: string]: { content: string } };

export type GistData = {
  files?: GistContent;
  message: string;
};

export default class GistFs {
  static loadData(gistId: string): Promise<GistData> {
    const gists = new Gists();

    return gists
      .get(gistId)
      .then((data: any) => {
        return data.body;
      })
      .then((data: any) => {
        return data as GistData;
      })
      .catch((e: any) => {
        throw new Error(JSON.stringify(e));
      });
  }

  static uploadData(
    data: { [a: string]: { content: string } },
    description: string,
    token: string,
    gistId?: string
  ): Promise<string> {
    if (token === "default") {
      const dataRequest: SourceFile[] = [];
      Object.keys(data).forEach((k) => {
        dataRequest.push({ filePath: k, source: data[k].content });
      });
      return WebIdeApi.createUpdateGist({
        description,
        gistId,
        sourceFiles: dataRequest,
      })
        .then((v) => v.data)
        .catch((e) => {
          throw new Error(e.message ? e.message : e);
        });
    }
    const gists = new Gists({ token });

    const action = gistId
      ? (options: any) => gists.edit(gistId, options)
      : (options: any) => gists.create(options);

    return action({ description, public: false, files: data })
      .then(
        (result: {
          body: { html_url: any };
          errors: any;
          message: string;
          documentation_url: any;
        }) => {
          if (result.body.html_url) {
            return result.body.html_url;
          }
          const error = JSON.stringify(result.errors, null, "\t") || "";
          const message =
            result.message === "Not Found"
              ? `${result.message}. Please make sure the API token has right to create a gist.`
              : result.message;
          throw new Error(`${message} ${result.documentation_url} ${error}`);
        }
      )
      .catch((error: any) => {
        throw new Error(error.message ? error.message : error);
      });
  }
}
