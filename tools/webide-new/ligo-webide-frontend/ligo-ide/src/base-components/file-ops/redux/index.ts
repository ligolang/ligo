export default {
  default: "default",
  persist: true,
  actions: {
    SET_GIST_TOKEN: {
      reducer: (_: any, { payload }: { payload: string }) => payload,
    },
  },
};
