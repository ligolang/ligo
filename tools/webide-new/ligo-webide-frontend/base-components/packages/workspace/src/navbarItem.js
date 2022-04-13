import platform from '@obsidians/platform'
import fileOps from '@obsidians/file-ops'
import actions from './actions'

const projectContextMenus = id => {
  if (id === 'new-project' || id === 'open-project') {
    return
  }

  if (platform.isWeb) {
    return
  }

  return [
    {
      text: `Open Containing Folder`,
      onClick: async project => {
        const result = await fileOps.current.openItem(project.path)

        if (result) {
          !result.exsist && actions.removeProject(project)
        }
      },
    },
    {
      text: `Open in Terminal`,
      onClick: async project => {
        const result = await fileOps.current.openInTerminal(project.path)
        if (result) {
          !result.exsist && actions.removeProject(project)
        }
      },
    },
    null,
    {
      text: `Remove`,
      onClick: project => actions.removeProject(project),
    },
  ]
}

export default function navbarItem(projects, selected, username = 'local') {
  if (platform.isWeb && username === 'local') {
    return {
      route: 'local',
      title: 'Project',
      icon: 'fas fa-file-code',
      selected,
      dropdown: [{ none: true }],
      contextMenu: projectContextMenus,
    }
  }

  const localProjects = projects.get('local')?.toJS() || []
  const remoteProjects = projects.get('remote')?.toJS() || []
  let projectDropdown
  if (platform.isDesktop) {
    projectDropdown = [
      { divider: true },
      { header: username === 'local' ? 'projects' : 'local projects' },
    ]
    if (localProjects.length) {
      projectDropdown = projectDropdown.concat(localProjects.map(p => ({ ...p, route: p.author })))
    } else {
      projectDropdown.push({ none: true })
    }

    if (username !== 'local') {
      projectDropdown = projectDropdown.concat([
        { divider: true },
        { header: 'remote projects' },
      ])
      if (remoteProjects.length) {
        projectDropdown = projectDropdown.concat(remoteProjects.map(p => ({ ...p, route: p.author })))
      } else {
        projectDropdown.push({ none: true })
      }
    }
  } else {
    projectDropdown = [
      { divider: true },
      { header: 'projects' },
    ]
    if (remoteProjects.length) {
      projectDropdown = projectDropdown.concat(remoteProjects.map(p => ({ ...p, route: p.author })))
    } else {
      projectDropdown.push({ none: true })
    }
  }

  if (platform.isDesktop) {
    projectDropdown.unshift({
      id: 'open-project',
      name: 'Open Project...',
      icon: 'fas fa-folder-plus',
      onClick: () => actions.openProject(),
    })
  }
  projectDropdown.unshift({
    id: 'new-project',
    name: 'Create Project...',
    icon: 'fas fa-plus',
    onClick: () => actions.newProject(platform.isWeb),
  })

  return {
    route: selected.author || username,
    title: 'Project',
    icon: 'fas fa-file-code',
    selected,
    dropdown: projectDropdown,
    contextMenu: projectContextMenus,
  }
}