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

let extract_pair : value -> (value * value , _) result =
  fun p ->
    ( match p with
      | V_Record lmap ->
        let fst = LMap.find (Label "0") lmap in
        let snd = LMap.find (Label "1") lmap in
        ok (fst,snd)
      | _ -> failwith "value is not a pair" )

let is_true : value -> (bool , _) result =
  fun b -> match b with
    | V_Ct (C_bool b) -> ok b
    | _ -> failwith "value is not a bool"
