import Gists from 'gists'

export type GistData = {
    files?: {[a: string]: string},
    message: string
}

export default class GistFs {
    async loadData(gistId: string): Promise<GistData> {
        return await fetch(`https://api.github.com/gists/${gistId}`)
          .then(data => data.json())
          .then(data => {
            return data as GistData
          })
          .catch(e => {
            throw new Error(JSON.stringify(e))
          })
    }

    async uploadData(data: {[a: string]: {content: string}}, description: string, token: string): Promise<string> {
        const gists = new Gists({ token })

        return await gists.create({description, public: true, files: data })
          .then(result => {
            if (result.body.html_url) {
              return result.body.html_url
            } else {
              const error = JSON.stringify(result.errors, null, '\t') || ''
              const message = result.message === 'Not Found' ? result.message + '. Please make sure the API token has right to create a gist.' : result.message
              throw new Error(message + ' ' + result.documentation_url + ' ' + error)
            }
          })
          .catch(error => {
            throw new Error(error.message ? error.message : error)
          })
    }
}
