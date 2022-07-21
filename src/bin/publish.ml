(* TODO:

spec:  Result {
  type: 'version',
  registry: true,
  where: undefined,
  raw: 'melwyn95_dummy@1.0.10',
  name: 'melwyn95_dummy',
  escapedName: 'melwyn95_dummy',
  scope: undefined,
  rawSpec: '1.0.10',
  saveSpec: null,
  fetchSpec: '1.0.10',
  gitRange: undefined,
  gitCommittish: undefined,
  hosted: undefined }

  opts = {
    defaultTag: 'latest',
    // if scoped, restricted by default
    access: spec.scope ? 'restricted' : 'public',
    algorithms: ['sha512'],
    ...opts,
    spec,
  }
  For now just implement `public` later do restricted 

registry:  https://registry.npmjs.org/ (Replace with LIGO registry)

patchManifest

buildMetadata

      method: 'PUT',
      body: metadata,
      ignoreBody: true,

      if faile: patchMetadata 
   
    References:
      https://github.com/npm/cli/blob/latest/workspaces/libnpmpublish/lib/publish.js
      https://github.com/npm/cli/blob/latest/node_modules/npm-registry-fetch/lib/index.js
      https://github.com/npm/cli/blob/latest/node_modules/npm-registry-fetch/lib/default-opts.js
      https://github.com/npm/cli/blob/latest/node_modules/npm-package-arg/lib/npa.js

*)

let tar dir = 
  let open Lwt.Syntax in
  let* fd = Lwt_unix.openfile "foo.tar" [ Unix.O_CREAT ; Unix.O_RDWR ] 0x777 in
  let* () = Tar_lwt_unix.Archive.create ["hack.js"] fd in
  let* () = Lwt_unix.close fd in
  Lwt.return ()

let publish ~ligo_registry =
  let cwd = Unix.getcwd () in
  let () = Lwt_main.run (tar cwd) in
  Ok ("", "")