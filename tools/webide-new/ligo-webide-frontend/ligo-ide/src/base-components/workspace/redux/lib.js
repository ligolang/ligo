function getId(project) {
  return project ? project.get("id") : undefined;
}

export function findIndex(state, id, type = "cache") {
  return state.get(type).findIndex(p => getId(p) === id);
}
