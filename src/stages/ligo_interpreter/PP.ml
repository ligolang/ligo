open Types

let rec pp_value : value -> string = function
  | V_Ct (C_int i) -> Format.asprintf "%i : int" i
  | V_Ct (C_nat n) -> Format.asprintf "%i : nat" n
  | V_Ct (C_string s) -> Format.asprintf "\"%s\" : string" s
  | V_Ct (C_unit) -> Format.asprintf "unit"
  | V_Ct (C_bool true) -> Format.asprintf "true"
  | V_Ct (C_bool false) -> Format.asprintf "false"
  | V_Ct (C_bytes b) -> Format.asprintf "0x%a : bytes" Hex.pp (Hex.of_bytes b)
  | V_Ct (C_mutez i) -> Format.asprintf "%i : mutez" i 
  | V_Ct _ -> Format.asprintf "PP, TODO"
  | V_Failure s -> Format.asprintf "\"%s\" : failure " s
  | V_Record recmap ->
    let content = LMap.fold (fun label field prev ->
      let (Label l) = label in
      Format.asprintf "%s ; %s = (%s)" prev l (pp_value field))
      recmap "" in
    Format.asprintf "{ %s }" content
  | V_Func_val _ -> Format.asprintf "<fun>"
  | V_Construct (name,v) -> Format.asprintf "%s(%s)" name (pp_value v)
  | V_List vl ->
    Format.asprintf "[ %s ]" @@
      List.fold_left (fun prev v -> Format.asprintf "%s ; %s" prev (pp_value v)) "" vl
  | V_Map vmap ->
    Format.asprintf "[ %s ]" @@
      List.fold_left (fun prev (k,v) -> Format.asprintf "%s ; %s -> %s" prev (pp_value k) (pp_value v)) "" vmap
  | V_Set slist ->
    Format.asprintf "{ %s }" @@
      List.fold_left (fun prev v -> Format.asprintf "%s ; %s" prev (pp_value v)) "" slist

let pp_env : env -> unit = fun env ->
  let () = Format.printf "{ #elements : %i\n" @@ Env.cardinal env in
  let () = Env.iter (fun var v ->
    Format.printf "\t%s -> %s\n" (Var.to_name var) (pp_value v))
  env in
  let () = Format.printf "\n}\n" in
  ()