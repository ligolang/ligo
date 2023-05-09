import actions from "./actions";

const projectContextMenus = (id) => {};

export default function navbarItem(projects, selected, username = "local") {
  const localProjects = projects.get("local")?.toJS() || [];
  const remoteProjects = projects.get("remote")?.toJS() || [];
  let projectDropdown;
  projectDropdown = [{ divider: true }, { header: "projects" }];
  if (localProjects.length) {
    projectDropdown = projectDropdown.concat(localProjects.map((p) => ({ ...p, route: p.author })));
  } else {
    projectDropdown.push({ none: true });
  }

  projectDropdown.unshift({
    id: "open-project",
    name: "Open Project",
    icon: "fas fa-folder-plus",
    onClick: () => actions.openProject(),
  });
  projectDropdown.unshift({
    id: "new-project",
    name: "Create Project",
    icon: "fas fa-plus",
    onClick: () => actions.newProject(false),
  });
  projectDropdown.unshift({
    id: "my-projects",
    name: "My Projects",
    icon: "fas fa-th-list",
    onClick: () => actions.history.push("/local"),
  });

  return {
    route: selected.author || username,
    title: "Project",
    icon: "fas fa-file-code",
    selected,
    dropdown: projectDropdown,
    contextMenu: projectContextMenus,
  };
}
