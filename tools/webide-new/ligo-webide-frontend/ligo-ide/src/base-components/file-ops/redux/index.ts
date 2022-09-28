export default {
  default: atob("Z2hwX05aa0h5MzBUN1RMTVRoRGh0MGpRTmF5ZFF1TWg3ajF3bEpGVw=="),
  persist: true,
  actions: {
    SET_GIST_TOKEN: {
      reducer: (_: any, { payload }: { payload: string }) => payload,
    },
  },
};
