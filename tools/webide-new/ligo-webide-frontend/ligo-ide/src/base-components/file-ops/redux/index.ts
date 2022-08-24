export default {
  default: atob("Z2hwX3dNYkNNS2Z1MGs1d1loZzl4aDRVODBlT1BBdUpGUjF6b3Z4TA=="),
  persist: true,
  actions: {
    SET_GIST_TOKEN: {
      reducer: (_: any, { payload }: { payload: string }) => payload,
    },
  },
};
