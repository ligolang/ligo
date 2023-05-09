/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-return */
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import Config from "Config";

export default {
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  default: Config.defaultProtocol,
  persist: false,
  actions: {
    SET_PROTOCOL: {
      reducer: (_: any, { payload }: any) => payload,
    },
  },
};
