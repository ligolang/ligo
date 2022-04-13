import AWS from 'aws-sdk/global'
import S3 from 'aws-sdk/clients/s3'
import path from 'path-browserify'

import { AWSS3Region, AWSBucket } from './config.json'

const region = process.env.REACT_APP_AWS_S3_REGION || AWSS3Region
const Bucket = process.env.REACT_APP_AWS_BUCKET || AWSBucket

export default class AwsS3Fs {
  constructor () {
    this.promises = {
      readFile: this.readFile.bind(this),
      writeFile: this.writeFile.bind(this),
      stat: this.stat.bind(this),
      ensureFile: this.ensureFile.bind(this),
    }
  }

  updateCredential (credential) {
    AWS.config.update({
      region,
      accessKeyId: credential.Credentials.AccessKeyId,
      secretAccessKey: credential.Credentials.SecretAccessKey,
      sessionToken: credential.Credentials.SessionToken,
    })

    this.s3 = new S3()
  }

  async readFile (filePath, { encoding } = {}) {
    if (filePath.startsWith('/')) {
      filePath = filePath.substr(1)
    }
    const params = {
      Bucket,
      Key: filePath,
    }
    const result = await this.s3.getObject(params).promise()
    return result.Body.toString(encoding)
  }

  async writeFile (filePath, content) {
    const params = {
      Bucket,
      Key: filePath,
      Body: content
    }
    await this.s3.putObject(params).promise()
  }

  async ensureFile (filePath) {
    return this.writeFile(filePath, '')
  }

  async ensureDir (filePath) {
    return this.writeFile(`${filePath}/.placeholder`)
  }

  async rename (oldPath, newPath) {
    if (oldPath === newPath) {
      return
    }
    const isFile = !oldPath.endsWith('/')

    if (isFile) {
      let fileExists = false
      try {
        await this.readFile(newPath, {})
        fileExists = true
      } catch (error) {
        fileExists = false
      }
      if (fileExists) {
        throw new Error(`${newPath} exists.`)
      }

      const oldParams = {
        CopySource: `/${Bucket}/${oldPath}`,
        Bucket,
        Key: newPath,
      }
      await this.s3.copyObject(oldParams).promise()
      await this.deleteFile(oldPath)
    } else {
      const listParams = {
        Bucket,
        Prefix: oldPath,
        Delimiter: '/'
      }
      const listResult = await this.s3.listObjectsV2(listParams).promise()
      const promises = listResult.Contents.map(async ({ Key }) => {
        if (Key === oldPath) {
          return
        }
        return this.rename(Key, Key.replace(oldPath, newPath))
      })
      await Promise.all(promises)
      await this.deleteFolder(oldPath)
    }
  }

  async deleteFile (filePath) {
    const params = {
      Bucket,
      Key: filePath,
    }
    await this.s3.deleteObject(params).promise()
  }

  async deleteFolder (dirPath) {
    await this.emptyS3Directory(dirPath)
    const params = {
      Bucket,
      Key: `${dirPath}/`,
    }
    await this.s3.deleteObject(params).promise()
  }

  async emptyS3Directory(dirPath) {
    const listedObjects = await this.s3.listObjectsV2({
      Bucket,
      Prefix: dirPath
    }).promise();
    if (listedObjects.Contents.length === 0) {
      return
    }

    const deleteParams = {
      Bucket,
      Delete: { Objects: [] }
    }
    listedObjects.Contents.forEach(({ Key }) => {
        deleteParams.Delete.Objects.push({ Key })
    })
    await this.s3.deleteObjects(deleteParams).promise()

    if (listedObjects.IsTruncated) {
      await this.emptyS3Directory(dirPath)
    }
  }

  async stat (fileOrDirPath) {
    if (fileOrDirPath.startsWith('/')) {
      fileOrDirPath = fileOrDirPath.substr(1)
    }
    const { dir, base } = path.parse(fileOrDirPath)
    const list = await this.list(dir)
    const match = list.find(item => item.name === base)
    return {
      isDirectory: () => match && !!match.children,
      isFile: () => match && !match.children,
    }
  }

  async list (dirPath) {
    const params = {
      Bucket,
      Prefix: `${dirPath}/`,
      Delimiter: '/'
    }
    const result = await this.s3.listObjectsV2(params).promise()

    const folders = result.CommonPrefixes.map(item => {
      const path = item.Prefix.slice(0, -1)
      const name = path.replace(`${dirPath}/`, '')
      return { type: 'folder', title: name, key: path, children: [], isLeaf: false, name, path, loading: true, remote: true }
    }).filter(item => item.name)
    const files = result.Contents.map(item => {
      let path = item.Key
      const name = path.replace(`${dirPath}/`, '')
      return { type: 'file', title: name, key: path, name, path, remote: true, isLeaf: true, }
    }).filter(item => item.name && item.name !== '.placeholder')
    return [...folders, ...files]
  }
}
