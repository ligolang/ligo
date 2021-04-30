(* External dependencies *)

module Region = Simple_utils.Region

(* Redefinitions *)

module type S =
  sig
    type token
    val process : token list -> token list
  end

type token = Token.t

let es6fun = Token.ES6FUN Region.ghost

(* Inserting the ES6FUN virtual token *)

let insert_es6fun_token tokens =
  let open Token in
  (* Unclosed parentheses are used to check if the parentheses are balanced *)
  let rec inner result unclosed_parentheses tokens =
    match tokens with
    (* Balancing parentheses *)

    | (RPAR _ as hd) :: rest ->
      inner (hd :: result) (unclosed_parentheses + 1) rest

    (* let foo = (b: (int, int) => int) => ... *)

    | (LPAR _ as hd) :: (COLON _ as c) :: (Ident _ as i) :: (LPAR _ as l) :: rest
         when unclosed_parentheses = 1 ->
      List.rev_append (l :: i :: c :: es6fun :: hd :: result) rest

    (* let a = (x:int) => x *)

    | (LPAR _ as hd) :: (ARROW _ as a) :: rest when unclosed_parentheses = 1 ->
      List.rev_append (a :: es6fun :: hd :: result) rest

    | (DOT _ as dot) :: (Constr _ as hd) :: rest ->
      inner (hd :: dot :: result) unclosed_parentheses rest
    | (_ as hd) :: (Constr _ as c) :: rest ->
      List.rev_append (c :: hd :: result) rest

    (* let foo = (a: int) => (b: int) => a + b *)

    | (_ as hd) :: (ARROW _ as a) :: rest when unclosed_parentheses = 0 ->
      List.rev_append (a :: es6fun :: hd :: result) rest

    (* ((a: int) => a *)

    | (LPAR _ as hd) :: (LPAR _ as a) :: rest when unclosed_parentheses = 1 ->
      List.rev_append (a :: es6fun :: hd :: result) rest

    (* let x : (int => int) *)

    | (LPAR _ as hd) :: rest when unclosed_parentheses = 0 ->
      List.rev_append (hd :: es6fun :: result) rest

    (* Balancing parentheses *)

    | (LPAR _ as hd) :: rest ->
      inner (hd :: result) (unclosed_parentheses - 1) rest

    (* When the arrow '=>' is not part of a function: *)

    | (RBRACKET _ as hd) :: rest
    | (C_Some _ as hd) :: rest
    | (C_None _ as hd) :: rest
    | (VBAR _ as hd) :: rest ->
      List.rev_append (hd :: result) rest

    (* let foo : int => int = (i: int) => ...  *)

    | (COLON _ as hd) :: (Ident _ as i) :: (Let _ as l) :: rest when unclosed_parentheses = 0 ->
      List.rev_append (l :: i :: hd :: es6fun :: result) rest

    | (EQ _ as hd) :: rest ->
      List.rev_append (hd :: es6fun :: result) rest
    | hd :: rest -> inner (hd :: result) unclosed_parentheses rest
    | []  -> List.rev result
  in
  inner [] 0 tokens

let process tokens =
  let open Token in
  let rec inner result t =
    match t with
     (ARROW _ as a) :: rest ->
       inner (insert_es6fun_token (a :: result)) rest
    | hd :: rest -> inner (hd :: result) rest
    | [] -> List.rev result
  in
  inner [] tokens
