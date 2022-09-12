/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-return */

export default {
  default: { showName: "Jakarta (mainnet)", name: "jakarta" },
  persist: false,
  actions: {
    SET_PROTOCOL: {
      reducer: (_: any, { payload }: any) => payload,
    },
  },
};
