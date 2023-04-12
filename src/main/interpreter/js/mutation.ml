let mutate_some_contract ~raise:_ ?syntax:_ _z _main =
  (* TODO(prometheansacrifice) mutation for browser *)
  None


let mutate_some_value ~raise:_ ?syntax:_ _loc _z _v _v_type =
  (* TODO(prometheansacrifice) mutation for browser *)
  None


let value_gen (* TODO(prometheansacrifice) mutation for browser *)
    ~raise:_
    ?small:_
    ?known_addresses:_
    _type_expr
  =
  Obj.magic 0


let buffer_of_mutation = Obj.magic 0
let get_mutation_id = Obj.magic 0
