let fail_data = "my contract always fail"

[@entry] let main () () : operation list * unit = [], failwith fail_data
