type ('a, 'b) runned_result =
  | Success of 'a
  | Fail of 'b (* that string represent michelson code*)

type check_type =
  | Check_parameter
  | Check_storage
