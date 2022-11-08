/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import { AxiosInstance, AxiosResponse } from "axios";
import redux from "~/base-components/redux";

export type ProtocolArg = {
  protocol?: string;
};

export type Project = {
  sourceFiles: { filePath: string; source: string }[];
  main: string;
};

export type CompileContractArgsApi = ProtocolArg & {
  project: Project;
};

export type GenerateDeployScriptArgsApi = CompileContractArgsApi & {
  name: string;
  storage: string;
};

export type DryRunArgsApi = CompileContractArgsApi & {
  entrypoint: string;
  storage: string;
  parameters: string;
};

export type CompileExpressionArgsApi = CompileContractArgsApi & {
  function: string;
};

export type ListDeclarationsArgsApi = CompileContractArgsApi & {
  onlyEndpoint: boolean;
};

interface LigoIdeApiInterface {
  compileContract(args: CompileContractArgsApi): Promise<string>;
  generateDeployScript(args: GenerateDeployScriptArgsApi): Promise<string>;
  dryRun(args: DryRunArgsApi): Promise<string>;
  listDeclarations(args: ListDeclarationsArgsApi): Promise<string[]>;
  compileExpression(args: CompileExpressionArgsApi): Promise<string>;
}

export default function LigoIdeApi(axiosInst: AxiosInstance): LigoIdeApiInterface {
  return {
    compileContract: async (argument: CompileContractArgsApi) => {
      const args = argument;
      const { protocol } = redux.getState();
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      args.protocol = protocol.name;
      return axiosInst.post("/compile", args).then((resp: AxiosResponse<string>) => resp.data);
    },
    generateDeployScript: async (argument: GenerateDeployScriptArgsApi) => {
      const args = argument;
      const { protocol } = redux.getState();
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      args.protocol = protocol.name;
      return axiosInst
        .post("/generate-deploy-script", args)
        .then((resp: AxiosResponse<string>) => resp.data);
    },
    dryRun: async (argument: DryRunArgsApi) => {
      const args = argument;
      const { protocol } = redux.getState();
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      args.protocol = protocol.name;
      return axiosInst.post("/dry-run", args).then((resp: AxiosResponse<string>) => resp.data);
    },
    listDeclarations: async (argument: ListDeclarationsArgsApi) => {
      const args = argument;
      const { protocol } = redux.getState();
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      args.protocol = protocol.name;
      return axiosInst
        .post("/list-declarations", args)
        .then((resp: AxiosResponse<string[]>) => resp.data);
    },
    compileExpression: async (args: CompileExpressionArgsApi) => {
      return axiosInst
        .post("/compile-expression", args)
        .then((resp: AxiosResponse<string>) => resp.data);
    },
  };
}
