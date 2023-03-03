/* eslint-disable @typescript-eslint/unbound-method */
import axios, { AxiosInstance } from "axios";
import {
  CompileExpressionRequest,
  CompileRequest,
  DefaultApiFactory,
  DryRunRequest,
  GenerateDeployScriptRequest,
} from "./generated";
import redux from "~/base-components/redux";

type ErrorBody = {
  reason: string;
  type: string;
  showToUser: boolean;
};

type ResponseWithData = {
  data: ErrorBody;
};

type ErrorWithResponse = {
  response: ResponseWithData;
};

const isErrorWithResponse = (obj: any): obj is ErrorWithResponse => {
  return typeof obj === "object" && "response" in obj;
};

const isResponseWithData = (obj: any): obj is ResponseWithData => {
  return typeof obj === "object" && "data" in obj;
};

const isErrorBody = (obj: any): obj is ErrorBody => {
  return typeof obj === "object" && "reason" in obj && "type" in obj && "showToUser" in obj;
};

const isBackendError = (obj: any): obj is ErrorWithResponse => {
  return (
    isErrorWithResponse(obj) && isResponseWithData(obj.response) && isErrorBody(obj.response.data)
  );
};

export const mkAxios = (): AxiosInstance => {
  const instance = axios.create();
  instance.interceptors.response.use(undefined, (error) => {
    if (isBackendError(error)) {
      if (error.response.data.showToUser) {
        throw new Error(error.response.data.reason);
      } else {
        console.error(error.response.data.type, error.response.data.reason);
        return Promise.reject(error);
      }
    }
    return Promise.reject(error);
  });
  instance.interceptors.request.use(undefined, (error) => {
    console.error(JSON.stringify(error));
    return Promise.reject(error);
  });
  return instance;
};

const baseUrl = `http://${process.env.BACKEND_URL!}`;

const addProtocol = <T extends { protocol?: string }>(args: T) => {
  const argsWithProtocol = args;
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const { protocol } = redux.getState();
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
  argsWithProtocol.protocol = protocol.name;
  return argsWithProtocol;
};

export const WebIdeApi = {
  compileExpression: (args: CompileExpressionRequest) =>
    DefaultApiFactory(undefined, baseUrl, mkAxios()).compileExpressionPost(addProtocol(args)),
  compileContract: (args: CompileRequest) =>
    DefaultApiFactory(undefined, baseUrl, mkAxios()).compilePost(addProtocol(args)),
  dryRun: (args: DryRunRequest) =>
    DefaultApiFactory(undefined, baseUrl, mkAxios()).dryRunPost(addProtocol(args)),
  generateDeployScript: (args: GenerateDeployScriptRequest) =>
    DefaultApiFactory(undefined, baseUrl, mkAxios()).generateDeployScriptPost(addProtocol(args)),
  listDeclarations: DefaultApiFactory(undefined, baseUrl, mkAxios()).listDeclarationsPost,
  createUpdateGist: DefaultApiFactory(undefined, baseUrl, mkAxios()).createUpdateGistPost,
  listTemplates: DefaultApiFactory(undefined, baseUrl, mkAxios()).listTemplatesPost,
  ligoVersion: DefaultApiFactory(undefined, baseUrl, mkAxios()).ligoVersionPost,
};
