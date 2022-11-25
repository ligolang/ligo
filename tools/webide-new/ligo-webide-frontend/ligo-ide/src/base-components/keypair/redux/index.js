import { Map } from "immutable";

export default {
  default: Map(),
  persist: true,
  actions: {
    UPDATE_FROM_REMOTE: {
      reducer: (state, { payload }) => {
        let reduceState = state;
        payload.forEach((keypair) => {
          if (typeof state.get(keypair.address) === "object") return;
          reduceState = reduceState.set(keypair.address, {
            address: keypair.address,
            name: keypair.name,
            balance: {},
          });
        });
        return reduceState;
      },
    },
    UPDATE_KEYPAIR_BALANCE: {
      reducer: (state, { payload }) => {
        return state.setIn([payload.address, "balance", payload.networkId], payload.balance);
      },
    },
    UPDATE_KEYPAIR: {
      reducer: (state, { payload }) => {
        const { address, name, secret = undefined, balance = {} } = payload;
        const key = state.get(payload.address);
        const newState = state.set(
          payload.address,
          key ? { address, name, secret: key.secret, balance } : { address, name, secret, balance }
        );
        return newState;
      },
    },
    REMOVE_KEYPAIR: {
      reducer: (state, { payload }) => {
        return state.remove(payload.address);
      },
    },
  },
};
