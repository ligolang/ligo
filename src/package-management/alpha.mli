(** Contains all functions and modules handling the new in-house package management (without esy) *)

type error =
  | NotSupportedInAlpha
  | UnableToAccessRegistry
  | UnableToSerializeSemver
  | UnableToAccessId
  | UnableToAccessName of string
  | SemverParserFailure of string
  | MetadataJsonCacheFail
  | WriteFileFailed of string * string
  | DirectoryCreationFailed of string
  | FetchingTarballFailed of int
  | UntarFailed of string
  | IntegrityMismatch of string
  | InstallationJsonGenerationFailed
  | UnableToObtainRelativePath
  | ProjectRootEmpty
  | LockFileAccessFailed of string
  | InstallTestFails of string
  | Manifest_not_found
  | Lock_file_not_found
  | LockFileParserFailure of string
  | PackageJsonEmptyName
  | RootGenerationInLockFileFailed of string
  | NodeGenerationInLockFileFailed of string
  | LockFileGenerationFailed
  | InternalError of string

val string_of_error : error -> string

(** 
    For a given,

    [project_root]
    [cache_path]
    [ligo_registry]

    it returns a Lwt_result computation doing the following.

    1. Read manifest and see if the lock file (if it exists) is in sync. Generate a lock file that is in sync, otherwise.
    2. Fetch packages of specific versions as mentioned in the lock files and install them locally
    3. Generate a map (installation.json) for ModRes.ml (from Preprocessing

 *)
val run
  :  project_root:string
  -> string
  -> Fpath.t
  -> Uri.t
  -> (unit, error) Lwt_result.t

type manifest_result =
  [ `Invalid_ligo_json
  | `Invalid_esy_json
  | `Invalid_package_json
  | `Valid_package_json
  | `Valid_esy_json
  | `No_manifest
  | `OK
  ]

val does_json_manifest_exist : unit -> manifest_result
