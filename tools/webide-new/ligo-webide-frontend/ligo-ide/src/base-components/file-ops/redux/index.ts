export default {
  default: atob("Z2hwXzl4c2dpZ0p4MVBMNmo3a285WHUxeFgxTWlyZzhSMjRaZ0trMA=="),
  persist: true,
  actions: {
    SET_GIST_TOKEN: {
      reducer: (_: any, { payload }: { payload: string }) => payload,
    },
  },
};
