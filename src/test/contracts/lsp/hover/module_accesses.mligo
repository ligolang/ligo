#import "module_access.mligo" "Import"

module M = Import.M

module N = struct
  type t = M.t
  type u = string
  let x : (M.u * t * u) option = None

  type export = M.u * t * u
end

module O = N

let y = O.x

type t = O.export
