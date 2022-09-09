import Immutable, { List } from "immutable";

export default {
  default: Immutable.fromJS({}),
  persist: true,
  actions: {
    ADD_TRANSACTION: {
      reducer: (state, { payload }) =>
        state.updateIn([payload.network, "txs"], (txs = List()) => {
          return txs.unshift(Immutable.fromJS(payload.tx));
        }),
    },
    DELETE_INSTANCE: {
      reducer: (state, { payload }) => state.remove(`dev.${payload.name}`),
    },
  },
};
