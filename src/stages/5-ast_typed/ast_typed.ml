module Types = Types
module Environment = Environment
module PP = PP
module PP_generic = PP_generic
module Compare_generic = Compare_generic
module Formatter = Formatter
module PP_json = PP_json
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
