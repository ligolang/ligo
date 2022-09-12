import { AxiosInstance, AxiosResponse } from "axios";

export interface CompileContractArgsApi {
  sources: [string, { unSource: string }][];
  main: string;
}

interface LigoIdeApiInterface {
  compileContract(args: CompileContractArgsApi): Promise<string>;
}

export default function LigoIdeApi(axiosInst: AxiosInstance): LigoIdeApiInterface {
  return {
    compileContract: async (args: CompileContractArgsApi) => {
      return axiosInst.post("/compile", args).then((resp: AxiosResponse<string>) => resp.data);
    },
  };
}
