module I_have_a_type = struct
  type here_it_is = string * int list
  let also_a_value : here_it_is = "hello", [42]
end

type the_type = I_have_a_type.