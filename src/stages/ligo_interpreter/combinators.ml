open Trace
open Types

let v_pair : value * value -> value =
  fun (a,b) -> V_Record (LMap.of_list [(Label "0", a) ; (Label "1",b)])

let v_bool : bool -> value =
  fun b -> V_Ct (C_bool b)

let v_unit : unit -> value =
  fun () -> V_Ct (C_unit)

let v_some : value -> value =
  fun v -> V_Construct ("Some", v)

let v_none : unit -> value =
  fun () -> V_Construct ("None", v_unit ())

let extract_pair : value -> (value * value) result =
  fun p ->
    let err = simple_error "value is not a pair" in
    ( match p with
      | V_Record lmap ->
        let%bind fst =  trace_option err @@
          LMap.find_opt (Label "0") lmap in
        let%bind snd =  trace_option err @@
          LMap.find_opt (Label "1") lmap in
        ok (fst,snd)
      | _ -> fail err )

let is_true : value -> bool result =
  fun b -> match b with
    | V_Ct (C_bool b) -> ok b
    | _ -> simple_fail "value is not a bool"
