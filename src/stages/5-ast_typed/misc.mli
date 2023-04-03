open Types

val assert_no_type_vars : type_expression -> unit option
val assert_type_expression_eq : type_expression * type_expression -> unit option
val type_expression_eq : type_expression * type_expression -> bool

module Free_variables : sig
  type bindings = Ligo_prim.Value_var.t list

  val lambda : bindings -> (expr, type_expression) Ligo_prim.Lambda.t -> bindings
end

val get_entry : program -> Ligo_prim.Value_var.t -> expression option
val assert_eq : 'a -> 'a -> unit option
val assert_list_eq : ('a -> 'a -> unit option) -> 'a list -> 'a list -> unit option
val get_type_of_contract : type_content -> (type_expression * type_expression) option
val build_entry_type : type_expression -> type_expression -> type_expression

val parameter_from_entrypoints
  :  (Ligo_prim.Value_var.t * type_expression) Simple_utils.List.Ne.t
  -> ( type_expression * type_expression
     , [> `Not_entry_point_form of Types.type_expression
       | `Storage_does_not_match of
         Ligo_prim.Value_var.t
         * Types.type_expression
         * Ligo_prim.Value_var.t
         * Types.type_expression
       ] )
     result

val should_uncurry_entry
  :  Types.type_expression
  -> [> `Bad
     | `No of Types.type_expression * Types.type_expression
     | `Yes of Types.type_expression * Types.type_expression
     ]

val fetch_views_in_program
  :  storage_ty:Types.type_expression
  -> Types.program
  -> Types.program
     * (Types.type_expression * Types.type_expression Ligo_prim.Binder.t) list

val uncurry_wrap
  :  loc:Location.t
  -> type_:Types.type_expression
  -> Types.expression_variable
  -> Types.expression option

val should_uncurry_view
  :  storage_ty:Types.type_expression
  -> Types.type_expression
  -> [> `Bad
     | `Bad_not_function
     | `Bad_storage of Types.type_expression
     | `No of Types.type_expression * Types.type_expression * Types.type_expression
     | `Yes of Types.type_expression * Types.type_expression * Types.type_expression
     ]
