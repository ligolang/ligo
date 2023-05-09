import mapValues from "lodash/mapValues";

const trackedActions = [
  "SET_VERSION",
  "UPDATE_PROFILE",
  "CLEAR_USER_PROFILE",
  "SELECT_PROJECT",
  "ADD_PROJECT",
  "REMOVE_PROJECT",
  "RENAME_PROJECT",
  "SET_GIST_TOKEN",
  "SET_PROTOCOL",
];

const middlewares = [];
if (process.env.NODE_ENV === "development") {
  const { createLogger } = require("redux-logger");
  middlewares.push(
    createLogger({
      collapsed: true,
      stateTransformer: (state) => mapValues(state, (s) => (s.toJS ? s.toJS() : s)),
    })
  );
}

export default middlewares;
