(* if -m is not given, the file isn't a contract and the file
  contains only _one_ contract module: let's assume we are compiling
  this module *)

module Single = struct
  [@entry] let main () () : operation list * unit = [],()
end