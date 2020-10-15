module Types = Types
module Environment = Environment
module PP = PP
module Yojson = To_yojson
module Formatter = Formatter
module Combinators = struct
  include Combinators
end
module Constant = Constant
module Misc = struct
  include Misc
  include Misc_smart
end
module Helpers = Helpers

include Types
include Misc
include Combinators
module Debug = Stage_common.Debug

let program_environment env program = fst (Compute_environment.program env program)
module Compare = struct include Compare end
