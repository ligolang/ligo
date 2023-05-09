import Immutable, { List } from "immutable";

export { redux as projects } from "~/base-components/workspace";
export { redux as keypairs } from "~/base-components/keypair";
export { redux as customNetworks } from "~/ligo-components/eth-network";
export { redux as gistToken } from "~/base-components/file-ops";
export { redux as protocol } from "~/ligo-components/eth-compiler";

export const version = {
  default: Immutable.fromJS({}),
  persist: true,
  actions: {
    SET_VERSION: {
      reducer: (state, { payload }) => state.merge(payload),
    },
  },
};

export const uiState = {
  default: Immutable.fromJS({}),
  persist: false,
  actions: {
    UPDATE_UI_STATE: {
      reducer: (state, { payload }) => state.merge(payload),
    },
  },
};

export const network = {
  default: "",
  persist: true,
  actions: {
    SELECT_NETWORK: {
      reducer: (_, { payload }) => payload,
    },
  },
};

export const customNetworkModalStatus = {
  default: false,
  persist: false,
  actions: {
    CUSTOM_MODAL_STATUS: {
      reducer: (_, { payload }) => payload,
    },
  },
};

export const networkConnect = {
  default: false,
  persist: false,
  actions: {
    CHANGE_NETWORK_STATUS: {
      reducer: (_, { payload }) => payload,
    },
  },
};
