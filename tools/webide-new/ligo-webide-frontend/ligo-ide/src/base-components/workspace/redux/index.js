import Immutable, { List, Map } from "immutable";
import { findIndex } from "./lib";

export default {
  default: Immutable.fromJS({
    selected: undefined,
    local: [],
  }),
  persist: true,
  actions: {
    SELECT_PROJECT: {
      reducer: (state, { payload }) => {
        const { project } = payload;
        return state.set("selected", Map({ ...project, loaded: false }));
      },
    },
    PROJECT_LOADED: {
      reducer: (state) => {
        return state.setIn(["selected", "loaded"], true);
      },
    },
    ADD_PROJECT: {
      reducer: (state, { payload }) => {
        const { type = "local", project } = payload;
        const index = findIndex(state, project.id, type);
        if (index === -1) {
          return state.update(type, (projects = List()) => projects.push(Map(project)));
        }
        return state;
      },
    },
    RENAME_PROJECT: {
      reducer: (state, { payload }) => {
        const { id, newName } = payload;
        let index = findIndex(state, id, "local");
        if (index > -1) {
          return state
            .update("local", (projects = List()) =>
              projects.set(
                index,
                Map({ id: newName, author: "local", name: newName, path: `.workspaces/${newName}` })
              )
            )
            .update("selected", (selected) => {
              if (selected && selected.get("id") === id) {
                return;
              }
              return selected;
            });
        }
        return state;
      },
    },
    REMOVE_PROJECT: {
      reducer: (state, { payload }) => {
        const { id } = payload;
        let index = findIndex(state, id, "local");
        if (index > -1) {
          return state
            .update("local", (projects = List()) => projects.remove(index))
            .update("selected", (selected) => {
              if (selected && selected.get("id") === id) {
                return;
              }
              return selected;
            });
        }

        index = findIndex(state, id, "remote");
        if (index > -1) {
          return state
            .update("remote", (projects = List()) => projects.remove(index))
            .update("selected", (selected) => {
              if (selected && selected.get("id") === id) {
                return;
              }
              return selected;
            });
        }

        return state;
      },
    },
    UPDATE_LOCAL_PROJECT_LIST: {
      reducer: (state, { payload }) => state.set("local", Immutable.fromJS(payload)),
    },
    UPDATE_REMOTE_PROJECT_LIST: {
      reducer: (state, { payload }) => state.set("remote", Immutable.fromJS(payload)),
    },
  },
};
