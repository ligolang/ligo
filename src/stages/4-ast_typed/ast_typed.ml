module Types = Types
module Environment = Environment
module PP = PP
module PP_generic = PP_generic
module Compare_generic = Compare_generic
module Combinators = struct
  include Combinators
end
module Misc = struct
  include Misc
  include Misc_smart
end
module Helpers = Helpers

include Types
include Misc
include Combinators

let program_environment env program = fst (Compute_environment.program env program)
