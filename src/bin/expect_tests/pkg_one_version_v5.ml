let body =
  {|{
  "name": "@foo/bar-pkg",
  "admin": "foo",
  "versions": {
    "1.0.5": {
      "name": "@foo/bar-pkg",
      "author": {
        "name": "foo bar"
      },
      "main": "./src/main.mligo",
      "type": "library",
      "storage_fn": null,
      "storage_arg": null,
      "repository": {
        "type": "git",
        "url": "https://github.com/foo/bar"
      },
      "version": "1.0.5",
      "description": "foo description",
      "scripts": {},
      "dependencies": {},
      "devDependencies": {
      },
      "bugs": {
        "url": "https://github.com/foo/bar/issues"
      },
      "_id": "@foo/bar-pkg@1.0.5",
      "dist": {
        "integrity": "sha512-MWUwYmRiOTY4MmQwYTFmNWJjNTYxYzJkMjQ2YzdhODUxYjZjZWZhNDJlNzQxMmIyYWZmYTliOGJmYjM0YzM2YWFjODdlYWRhYjg1YjM0MmFiMTY2ZjY1MWY3YWI5MjJlNzY2MGNjMTc1OGZkYmMwNDYyNTgyYzkwMmUwMmVjNjg=",
        "shasum": "5e1a7565d1bbe23aa89aaf502fe549c182a3ac93",
        "tarball": "http://localhost:4000/-/api/@foo/bar-pkg/-/@foo/bar-pkg-1.0.5.tgz",
        "fileCount": 54,
        "unpackedSize": 700895
      },
      "contributors": []
    },
    
  },
  "time": {
    "created": "2023-08-09T12:21:10.409Z",
    "modified": "2023-08-10T12:41:02.429Z",
    "1.0.5": "2023-08-10T12:39:08.964Z",
  },
  "dist-tags": {
    "latest": "1.0.5"
  },
  "_uplinks": {},
  "_distfiles": {},
  "_attachments": {
    "@foo/bar-pkg-1.0.5.tgz": {
      "shasum": "5e1a7565d1bbe23aa89aaf502fe549c182a3ac93"
    },
  },
  "_rev": "17-cf860d9e8aeb5547",
  "readme": "# foo ",
  "_id": "@foo/bar-pkg"
}|}
