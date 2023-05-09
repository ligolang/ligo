import redux from "~/base-components/redux";

export default class NavGuard {
  constructor(history) {
    this.history = history;
    const { location } = history;
    this.uninstall = history.listen((location) => this.handleChanges(location));
    this.handleChanges(location);
  }

  unmount() {
    this.uninstall();
  }

  handleChanges(location) {
    const { pathname } = location;

    const [first] = this.parsePathname(pathname);
    if (first !== "network") {
      this.updateSelectedProject(pathname);
    }
  }

  parsePathname(pathname) {
    const [_, ...args] = pathname.split("/");
    return args.map((x) => x || "");
  }

  updateSelectedProject(pathname) {
    const [author, id] = this.parsePathname(pathname);

    const { projects } = redux.getState();
    const oldSelected = projects.get("selected");
    if (oldSelected && oldSelected.get("author") === author && oldSelected.get("id") === id) {
      return;
    }

    const project = { pathname, author, id };

    // try to find projects from local
    const found = projects.get("local").find((p) => p.get("id") === id);
    if (found) {
      project.name = found.get("name");
      project.path = found.get("path");
    } else if (id) {
      project.name = `${author}/${id}`;
    }
    redux.dispatch("SELECT_PROJECT", { project });
  }
}
