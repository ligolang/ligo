import platform from '~/base-components/platform'
import actions from './actions'

const projectContextMenus = id => {
  return
}

export default function navbarItem(projects, selected, username = 'local') {
  // if (platform.isWeb && username === 'local') {
  //   return {
  //     route: 'local',
  //     title: 'Project',
  //     icon: 'fas fa-file-code',
  //     selected,
  //     dropdown: [{ none: true }],
  //     contextMenu: projectContextMenus
  //   }
  // }

  const localProjects = projects.get('local')?.toJS() || []
  const remoteProjects = projects.get('remote')?.toJS() || []
  let projectDropdown
  if (platform.isDesktop) {
    projectDropdown = [
      { divider: true },
      { header: username === 'local' ? 'projects' : 'local projects' }
    ]
    if (localProjects.length) {
      projectDropdown = projectDropdown.concat(localProjects.map(p => ({ ...p, route: p.author })))
    } else {
      projectDropdown.push({ none: true })
    }

    if (username !== 'local') {
      projectDropdown = projectDropdown.concat([
        { divider: true },
        { header: 'remote projects' }
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
      { header: 'projects' }
    ]
    if (localProjects.length) {
      projectDropdown = projectDropdown.concat(localProjects.map(p => ({ ...p, route: p.author })))
    } else {
      projectDropdown.push({ none: true })
    }
  }

  projectDropdown.unshift({
    id: 'open-project',
    name: 'Open Project',
    icon: 'fas fa-folder-plus',
    onClick: () => actions.openProject()
  })
  projectDropdown.unshift({
    id: 'new-project',
    name: 'Create Project',
    icon: 'fas fa-plus',
    onClick: () => actions.newProject(false)
  })

  return {
    route: selected.author || username,
    title: 'Project',
    icon: 'fas fa-file-code',
    selected,
    dropdown: projectDropdown,
    contextMenu: projectContextMenus
  }
}
