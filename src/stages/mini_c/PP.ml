[@@@coverage exclude_file]
open Simple_utils.PP_helpers
open Types
open Format

let list_sep_d x = list_sep x (const " , ")

let space_sep ppf () = fprintf ppf " "

let lr = fun ppf -> function `Left -> fprintf ppf "L" | `Right -> fprintf ppf "R"

let rec type_variable ppf : type_value -> _ = function
  | T_or(a, b) -> fprintf ppf "(%a) | (%a)" annotated a annotated b
  | T_pair(a, b) -> fprintf ppf "(%a) & (%a)" annotated a annotated b
  | T_base b -> type_constant ppf b
  | T_function(a, b) -> fprintf ppf "(%a) -> (%a)" type_variable a type_variable b
  | T_map(k, v) -> fprintf ppf "map(%a -> %a)" type_variable k type_variable v
  | T_big_map(k, v) -> fprintf ppf "big_map(%a -> %a)" type_variable k type_variable v
  | T_list(t) -> fprintf ppf "list(%a)" type_variable t
  | T_set(t) -> fprintf ppf "set(%a)" type_variable t
  | T_option(o) -> fprintf ppf "option(%a)" type_variable o
  | T_contract(t) -> fprintf ppf "contract(%a)" type_variable t

and annotated ppf : type_value annotated -> _ = function
  | (Some ann, a) -> fprintf ppf "(%a %%%s)" type_variable a ann
  | (None, a) -> type_variable ppf a

and environment_element ppf ((n, tv) : environment_element) =
  Format.fprintf ppf "%a : %a" Var.pp n type_variable tv

and environment ppf (x:environment) =
  fprintf ppf "Env[%a]" (list_sep_d environment_element) x

and type_constant ppf (tc:type_constant) : unit =
  let s = match tc with 
    | TC_unit      -> "unit"
    | TC_string    -> "string"
    | TC_bytes     -> "bytes"
    | TC_nat       -> "nat"
    | TC_int       -> "int"
    | TC_mutez     -> "mutez"
    | TC_bool      -> "bool"
    | TC_operation -> "operation"
    | TC_address   -> "address"
    | TC_key       -> "key"
    | TC_key_hash  -> "key_hash"
    | TC_signature -> "signatuer"
    | TC_timestamp -> "timestamp"
    | TC_chain_id  -> "chain_id"
    | TC_void      -> "void"
    in
  fprintf ppf "(TC %s)" s

let rec value ppf : value -> unit = function
  | D_bool b -> fprintf ppf "%b" b
  | D_operation _ -> fprintf ppf "operation[...bytes]"
  | D_int n -> fprintf ppf "%d" n
  | D_nat n -> fprintf ppf "+%d" n
  | D_timestamp n -> fprintf ppf "+%d" n
  | D_mutez n -> fprintf ppf "%dmutez" n
  | D_unit -> fprintf ppf "unit"
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes x ->
     fprintf ppf "0x%a" Hex.pp @@ Hex.of_bytes x
  | D_pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
  | D_left a -> fprintf ppf "L(%a)" value a
  | D_right b -> fprintf ppf "R(%a)" value b
  | D_none -> fprintf ppf "None"
  | D_some s -> fprintf ppf "Some (%a)" value s
  | D_map m -> fprintf ppf "Map[%a]" (list_sep_d value_assoc) m
  | D_big_map m -> fprintf ppf "Big_map[%a]" (list_sep_d value_assoc) m
  | D_list lst -> fprintf ppf "List[%a]" (list_sep_d value) lst
  | D_set lst -> fprintf ppf "Set[%a]" (list_sep_d value) lst

and value_assoc ppf : (value * value) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" value a value b

and expression ppf (e:expression) =
  fprintf ppf "%a" expression' e.content

and expression' ppf (e:expression') = match e with
  | E_skip -> fprintf ppf "skip"
  | E_closure x -> fprintf ppf "C(%a)" function_ x
  | E_variable v -> fprintf ppf "V(%a)" Var.pp v
  | E_application(a, b) -> fprintf ppf "(%a)@(%a)" expression a expression b

  | E_constant c -> fprintf ppf "%a %a" constant c.cons_name (pp_print_list ~pp_sep:space_sep expression) c.arguments
  | E_literal v -> fprintf ppf "L(%a)" value v
  | E_make_empty_map _ -> fprintf ppf "map[]"
  | E_make_empty_big_map _ -> fprintf ppf "big_map[]"
  | E_make_empty_list _ -> fprintf ppf "list[]"
  | E_make_empty_set _ -> fprintf ppf "set[]"
  | E_make_none _ -> fprintf ppf "none"
  | E_if_bool (c, a, b) -> fprintf ppf "%a ? %a : %a" expression c expression a expression b
  | E_if_none (c, n, ((name, _) , s)) -> fprintf ppf "%a ?? %a : %a -> %a" expression c expression n Var.pp name expression s
  | E_if_cons (c, n, (((hd_name, _) , (tl_name, _)) , cons)) -> fprintf ppf "%a ?? %a : (%a :: %a) -> %a" expression c expression n Var.pp hd_name Var.pp tl_name expression cons
  | E_if_left (c, ((name_l, _) , l), ((name_r, _) , r)) ->
      fprintf ppf "%a ?? %a -> %a : %a -> %a" expression c Var.pp name_l expression l Var.pp name_r expression r
  | E_sequence (a , b) -> fprintf ppf "%a ;; %a" expression a expression b
  | E_let_in ((name , _) , inline, expr , body) ->
      fprintf ppf "let %a = %a%a in ( %a )" Var.pp name expression expr option_inline inline expression body
  | E_iterator (b , ((name , _) , body) , expr) ->
      fprintf ppf "for_%a %a of %a do ( %a )" constant b Var.pp name expression expr expression body
  | E_fold (((name , _) , body) , collection , initial) ->
      fprintf ppf "fold %a on %a with %a do ( %a )" expression collection expression initial Var.pp name expression body

  | E_assignment (r , path , e) ->
      fprintf ppf "%a.%a := %a" Var.pp r (list_sep lr (const ".")) path expression e
  | E_record_update (r, path,update) ->
      fprintf ppf "%a with { %a = %a }" expression r (list_sep lr (const ".")) path expression update
  | E_while (e , b) ->
      fprintf ppf "while %a do %a" expression e expression b

and expression_with_type : _ -> expression -> _  = fun ppf e ->
  fprintf ppf "%a : %a"
    expression' e.content
    type_variable e.type_value

and function_ ppf ({binder ; body}:anon_function) =
  fprintf ppf "fun %a -> (%a)"
    Var.pp binder
    expression body

and assignment ppf ((n, i, e):assignment) = fprintf ppf "%a = %a%a;" Var.pp n expression e option_inline i

and option_inline ppf inline = 
  if inline then 
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and declaration ppf ((n,i, e):assignment) = fprintf ppf "let %a = %a%a;" Var.pp n expression e option_inline i

and tl_statement ppf (ass, _) = assignment ppf ass

and program ppf (p:program) =
  fprintf ppf "Program:\n---\n%a" (pp_print_list ~pp_sep:pp_print_newline tl_statement) p

and constant ppf : constant' -> unit = function
  | C_INT                   -> fprintf ppf "INT"
  | C_UNIT                  -> fprintf ppf "UNIT"
  | C_NIL                   -> fprintf ppf "NIL"
  | C_NOW                   -> fprintf ppf "NOW"
  | C_IS_NAT                -> fprintf ppf "IS_NAT"
  | C_SOME                  -> fprintf ppf "SOME"
  | C_NONE                  -> fprintf ppf "NONE"
  | C_ASSERTION             -> fprintf ppf "ASSERTION"
  | C_ASSERT_INFERRED       -> fprintf ppf "ASSERT_INFERRED"
  | C_FAILWITH              -> fprintf ppf "FAILWITH"
  | C_UPDATE                -> fprintf ppf "UPDATE"
  (* Loops *)
  | C_FOLD                  -> fprintf ppf "FOLD"
  | C_FOLD_WHILE            -> fprintf ppf "FOLD_WHILE"
  | C_CONTINUE              -> fprintf ppf "CONTINUE"
  | C_STOP                  -> fprintf ppf "STOP"
  | C_ITER                  -> fprintf ppf "ITER"
  (* MATH *)
  | C_NEG                   -> fprintf ppf "NEG"
  | C_ABS                   -> fprintf ppf "ABS"
  | C_ADD                   -> fprintf ppf "ADD"
  | C_SUB                   -> fprintf ppf "SUB"
  | C_MUL                   -> fprintf ppf "MUL"
  | C_DIV                   -> fprintf ppf "DIV"
  | C_MOD                   -> fprintf ppf "MOD"
  (* LOGIC *)
  | C_NOT                   -> fprintf ppf "NOT"
  | C_AND                   -> fprintf ppf "AND"
  | C_OR                    -> fprintf ppf "OR"
  | C_XOR                   -> fprintf ppf "XOR"
  (* COMPARATOR *)
  | C_EQ                    -> fprintf ppf "EQ"
  | C_NEQ                   -> fprintf ppf "NEQ"
  | C_LT                    -> fprintf ppf "LT"
  | C_GT                    -> fprintf ppf "GT"
  | C_LE                    -> fprintf ppf "LE"
  | C_GE                    -> fprintf ppf "GE"
  (* Bytes/ String *)
  | C_SIZE                  -> fprintf ppf "SIZE"
  | C_CONCAT                -> fprintf ppf "CONCAT"
  | C_SLICE                 -> fprintf ppf "SLICE"
  | C_BYTES_PACK            -> fprintf ppf "BYTES_PACK"
  | C_BYTES_UNPACK          -> fprintf ppf "BYTES_UNPACK"
  | C_CONS                  -> fprintf ppf "CONS"
  (* Pair *)
  | C_PAIR                  -> fprintf ppf "PAIR"
  | C_CAR                   -> fprintf ppf "CAR"
  | C_CDR                   -> fprintf ppf "CDR"
  | C_LEFT                  -> fprintf ppf "LEFT"
  | C_RIGHT                 -> fprintf ppf "RIGHT"
  | C_LSL                   -> fprintf ppf "LSL"
  | C_LSR                   -> fprintf ppf "LSR"
  (* Set *)
  | C_SET_EMPTY             -> fprintf ppf "SET_EMPTY"
  | C_SET_LITERAL           -> fprintf ppf "SET_LITERAL"
  | C_SET_ADD               -> fprintf ppf "SET_ADD"
  | C_SET_REMOVE            -> fprintf ppf "SET_REMOVE"
  | C_SET_ITER              -> fprintf ppf "SET_ITER"
  | C_SET_FOLD              -> fprintf ppf "SET_FOLD"
  | C_SET_MEM               -> fprintf ppf "SET_MEM"
  (* List *)
  | C_LIST_ITER             -> fprintf ppf "LIST_ITER"
  | C_LIST_MAP              -> fprintf ppf "LIST_MAP"
  | C_LIST_FOLD             -> fprintf ppf "LIST_FOLD"
  | C_LIST_CONS             -> fprintf ppf "LIST_CONS"
  (* Maps *)
  | C_MAP                   -> fprintf ppf "MAP"
  | C_MAP_EMPTY             -> fprintf ppf "MAP_EMPTY"
  | C_MAP_LITERAL           -> fprintf ppf "MAP_LITERAL"
  | C_MAP_GET               -> fprintf ppf "MAP_GET"
  | C_MAP_GET_FORCE         -> fprintf ppf "MAP_GET_FORCE"
  | C_MAP_ADD               -> fprintf ppf "MAP_ADD"
  | C_MAP_REMOVE            -> fprintf ppf "MAP_REMOVE"
  | C_MAP_UPDATE            -> fprintf ppf "MAP_UPDATE"
  | C_MAP_ITER              -> fprintf ppf "MAP_ITER"
  | C_MAP_MAP               -> fprintf ppf "MAP_MAP"
  | C_MAP_FOLD              -> fprintf ppf "MAP_FOLD"
  | C_MAP_MEM               -> fprintf ppf "MAP_MEM"
  | C_MAP_FIND              -> fprintf ppf "MAP_FIND"
  | C_MAP_FIND_OPT          -> fprintf ppf "MAP_FIND_OP"
  (* Big Maps *)
  | C_BIG_MAP               -> fprintf ppf "BIG_MAP"
  | C_BIG_MAP_EMPTY         -> fprintf ppf "BIG_MAP_EMPTY"
  | C_BIG_MAP_LITERAL       -> fprintf ppf "BIG_MAP_LITERAL"
  (* Crypto *)
  | C_SHA256                -> fprintf ppf "SHA256"
  | C_SHA512                -> fprintf ppf "SHA512"
  | C_BLAKE2b               -> fprintf ppf "BLAKE2b"
  | C_HASH                  -> fprintf ppf "HASH"
  | C_HASH_KEY              -> fprintf ppf "HASH_KEY"
  | C_CHECK_SIGNATURE       -> fprintf ppf "CHECK_SIGNATURE"
  | C_CHAIN_ID              -> fprintf ppf "CHAIN_ID"
  (* Blockchain *)
  | C_CALL                  -> fprintf ppf "CALL"
  | C_CONTRACT              -> fprintf ppf "CONTRACT"
  | C_CONTRACT_ENTRYPOINT   -> fprintf ppf "CONTRACT_ENTRYPOINT"
  | C_CONTRACT_OPT          -> fprintf ppf "CONTRACT OPT"
  | C_CONTRACT_ENTRYPOINT_OPT -> fprintf ppf "CONTRACT_ENTRYPOINT OPT"
  | C_AMOUNT                -> fprintf ppf "AMOUNT"
  | C_BALANCE               -> fprintf ppf "BALANCE"
  | C_SOURCE                -> fprintf ppf "SOURCE"
  | C_SENDER                -> fprintf ppf "SENDER"
  | C_ADDRESS               -> fprintf ppf "ADDRESS"
  | C_SELF_ADDRESS          -> fprintf ppf "SELF_ADDRESS"
  | C_IMPLICIT_ACCOUNT      -> fprintf ppf "IMPLICIT_ACCOUNT"
  | C_STEPS_TO_QUOTA        -> fprintf ppf "STEPS_TO_QUOTA"
  | C_SET_DELEGATE          -> fprintf ppf "SET_DELEGATE"

let%expect_test _ =
  Format.printf "%a" value (D_bytes (Bytes.of_string "foo")) ;
  [%expect{| 0x666f6f |}]

let%expect_test _ =
  let pp = expression' Format.std_formatter in
  let dummy_type = T_base TC_unit in
  let wrap e = { content = e ; type_value = dummy_type } in
  pp @@ E_closure { binder = Var.of_name "y" ; body = wrap (E_variable (Var.of_name "y")) } ;
  [%expect{|
    C(fun y -> (V(y)))
  |}] ;
  pp @@ E_closure { binder = Var.of_name "z" ; body = wrap (E_variable (Var.of_name "z")) } ;
  [%expect{|
    C(fun z -> (V(z)))
  |}]
