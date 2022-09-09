/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import { AxiosInstance, AxiosResponse } from "axios";
import redux from "~/base-components/redux";

export type ProtocolArg = {
  protocol?: string;
};

export type CompileContractArgsApi = ProtocolArg & {
  sources: [string, string][];
  main: string;
};

interface LigoIdeApiInterface {
  compileContract(args: CompileContractArgsApi): Promise<string>;
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
  };
}
