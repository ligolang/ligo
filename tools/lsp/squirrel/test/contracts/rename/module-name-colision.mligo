module Top = struct
  let some_name : string = "Don't rename me"
end

module Top = struct
  let some_name : string = "Rename me"
end

let main (_, _ : string * string): operation list * string =
  [], Top.some_name
