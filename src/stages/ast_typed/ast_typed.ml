module Types = Types
module Environment = Environment
module PP = PP
module Combinators = struct
  include Combinators
  include Combinators_environment
end
module Misc = struct
  include Misc
  include Misc_smart
end

include Types
include Misc
include Combinators
