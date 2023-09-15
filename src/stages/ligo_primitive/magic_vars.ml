open Var

module Magic_strings = struct
  let main = "main"
  let generated_main = "$main"
  let parameter = "$parameter"
  let storage = "$storage"
  let views = "$views"
  let contract = "$contract"
  let initial_dynamic_entrypoints = "$initial_dynamic_entrypoints"
end

open struct
  let loc = Location.generated
  let mk_v = Value_var.of_input_var ~loc
  let mk_t = Type_var.of_input_var ~loc
end

let main = mk_v Magic_strings.main
let parameter = mk_t Magic_strings.parameter
let storage = mk_t Magic_strings.storage
let views = mk_v Magic_strings.views
let contract = mk_v Magic_strings.contract
let generated_main = mk_v Magic_strings.generated_main
let initial_dynamic_entrypoints = mk_v Magic_strings.initial_dynamic_entrypoints
