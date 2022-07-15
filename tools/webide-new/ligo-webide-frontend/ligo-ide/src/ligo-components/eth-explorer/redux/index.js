import { Map } from "immutable";

export default {
  default: Map(),
  persist: true,
  actions: {
    ADD_TOKEN_INFO: {
      reducer: (state, { payload }) => {
        return state.setIn([payload.network, payload.address], Map(payload.tokenInfo));
      },
    },
    REMOVE_TOKEN_INFO: {
      reducer: (state, { payload }) => {
        return state.removeIn([payload.network, payload.address]);
      },
    },
  },
};
