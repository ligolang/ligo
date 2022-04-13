import { Map } from 'immutable'

export default {
  default: Map({}),
  persist: true,
  actions: {
    UPDATE_PROFILE: {
      reducer: (state, { payload }) => state.merge(payload)
    },
    CLEAR_USER_PROFILE: {
      reducer: () => Map({})
    },
    SET_VERSION: {
      reducer: (state, { payload }) => {
        if (payload.distinctId) {
          return state.merge({ distinctId: payload.distinctId })
        }
        return state
      }
    }
  }
}
