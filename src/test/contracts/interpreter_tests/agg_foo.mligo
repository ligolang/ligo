type case =
  {
   case_name : string;
   case_desc : string
  }

type suite =
  {
   suite_name : string;
   suite_cases : case list
  }

let case (name : string) (desc : string) =
  {
   case_name = name;
   case_desc = desc
  }

let suite (name : string) (cases : case list) : suite =
  {
   suite_name = name;
   suite_cases = cases
  }

let run_suite (suite : suite) =
  let () = Test.println ("Running " ^ "<" ^ suite.suite_name ^ ">") in
  ()
