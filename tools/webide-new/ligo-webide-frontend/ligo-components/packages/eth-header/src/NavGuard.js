import redux from '@obsidians/redux'

export default class NavGuard {
  constructor (history) {
    this.history = history
    const { location } = history
    this.uninstall = history.listen(location => this.handleChanges(location))
    this.handleChanges(location)
  }

  unmount () {
    this.uninstall()
  }

  handleChanges (location) {
    const { pathname } = location

    if (!this.preflight(pathname)) {
      return
    }

    const [first] = this.parsePathname(pathname)
    if (first === 'contract') {
      this.updateSelectedContract(pathname)
    } else if (first === 'account') {
      this.updateSelectedAccount(pathname)
    } else if (first !== 'network') {
      this.updateSelectedProject(pathname)
    }
  }

  preflight (pathname) {
    const state = redux.getState()

    if (pathname === '/') {
      // go to seleted project
      const selected = state.projects.get('selected')
      if (selected) {
        const author = selected.get('author')
        const id = selected.get('id')
        if (author && id) {
          this.history.replace(`/${author}/${id}`)
          return false
        }
      }
    }

    return true
  }

  parsePathname (pathname) {
    const [_, ...args] = pathname.split('/')
    return args.map(x => x || '')
  }

  updateSelectedContract (pathname) {
    const [_, ...rest] = this.parsePathname(pathname)
    const { network } = redux.getState()
    redux.dispatch('SELECT_CONTRACT', { network, contract: rest.join('/') })
  }

  updateSelectedAccount (pathname) {
    const [_, ...rest] = this.parsePathname(pathname)
    const { network } = redux.getState()
    redux.dispatch('SELECT_ACCOUNT', { network, account:  rest.join('/') })
  }

  updateSelectedProject (pathname) {
    const [author, id] = this.parsePathname(pathname)

    const { projects } = redux.getState()
    const oldSelected = projects.get('selected')
    if (
      oldSelected &&
      oldSelected.get('author') === author &&
      oldSelected.get('id') === id
    ) {
      return
    }

    const project = { pathname, author, id }
  
    // try to find projects from local
    const found = projects.get('local').find(p => p.get('id') === id)
    if (found) {
      project.name = found.get('name')
      project.path = found.get('path')
    } else if (id) {
      project.name = `${author}/${id}`
    }
    redux.dispatch('SELECT_PROJECT', { project })
  }
}