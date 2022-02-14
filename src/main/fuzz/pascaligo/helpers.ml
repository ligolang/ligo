include Fuzz_shared.Monad
open Cst.Pascaligo

module Fold_helpers(M : Monad) = struct
  open Monad_context(M)

  type 'a monad = 'a t
  let ok x = return x

  let nseq_to_list (hd, tl) = hd :: tl

  let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

  let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)
  let bind_map_npseq f (hd,tl) =
    let* hd = f hd in
    let* tl = bind_map_list (fun (a,b) -> let* b = f b in ok (a,b)) tl
    in ok (hd,tl)

  let bind_fold_npseq f init (hd,tl) =
    let* res = f init hd in
    let* res = bind_fold_list (fun init (_,b) -> f init b) res tl in
    ok res

  let pseq_to_list = function
    | None -> []
    | Some lst -> npseq_to_list lst
  let bind_map_pseq f = bind_map_option @@ bind_map_npseq f
  let bind_fold_pseq f init seq =
    let* res = bind_map_option (bind_fold_npseq f init) seq in
    ok @@ Option.value ~default:(init) res


  type 'a folder = {
      e : 'a -> expr -> 'a monad ;
      s : 'a -> statement -> 'a monad ;
      t : 'a -> type_expr -> 'a monad ;
      d : 'a -> declaration -> 'a monad ;
    }

  let rec fold_type_expression : 'a folder -> 'a -> type_expr -> 'a monad = fun f init t ->
    let self = fold_type_expression f in
    let* init = f.t init t in
    match t with
    | T_Attr (_,ty) -> self init ty
    | T_Cart {value = (ty,_,lst) ; region=_} ->
      let* init = self init ty in
      let aux init ty = self init ty in
      bind_fold_npseq aux init lst
    | T_Sum    {value;region=_} ->
       let {lead_vbar=_;variants} = value in
       let aux init ({value;region=_} : _ reg) =
         let {ctor_args;_} = value in
         match ctor_args with
           Some (_,t) -> self init t
         | None -> ok @@ init
       in
       bind_fold_ne_list aux init @@ npseq_to_ne_list variants
    | T_Record {value;region=_} ->
       let aux init ({value;region=_} : _ reg) =
         let {field_name=_;field_type;attributes=_} = value in
         match field_type with
         | Some (_,ty) -> self init ty
         | None -> ok init
       in
       bind_fold_pseq aux init value.elements
    | T_App {value;region=_} ->
      let (ty, tuple) = value in
      let* init = self init ty in
      bind_fold_ne_list self init @@ npseq_to_ne_list tuple.value.inside
    | T_Fun    {value;region=_} ->
       let (ty1, _, ty2) = value in
       let* res = self init ty1 in
       let* res = self res  ty2 in
       ok @@ res
    | T_Par    {value;region=_} ->
       self init value.inside
    | T_ModPath {value;region=_} ->
       self init value.field
    | T_Var    _ | T_Int    _ | T_String _ -> ok @@ init

  let rec fold_expression : 'a folder -> 'a -> expr -> 'a monad = fun f init e  ->
    let self = fold_expression f in
    let self_type = fold_type_expression f in
    let* init = f.e init e in
    let bin_op value =
      let {op=_;arg1;arg2} = value in
      let* res = fold_expression f init arg1 in
      let* res = fold_expression f res  arg2 in
      ok @@ res
    in
    match e with
    | E_Case    {value;region=_} ->
      let {kwd_case=_;expr;kwd_of=_;opening=_;lead_vbar=_;cases;closing=_} = value in
      let* res = self init expr in
      let* res = matching_cases self res cases in
      ok @@ res
    | E_Cond    {value;region=_} ->
      let ({test;if_so;if_not;_} : _ conditional) = value in
      let* res = self init test in
      let* res = self res if_so in
      let* res = match if_not with
        | Some (_,if_not) -> self res if_not
        | None -> ok res
      in
       ok @@ res
    | E_Typed   {value;region=_} ->
      let (expr, (_,ty)) = value.inside in
      let* res = self init expr in
      let* res = self_type res ty in
       ok res
    | E_Or  {value;region=_} -> bin_op value
    | E_And {value;region=_} -> bin_op value
    | E_Not {value;region=_} ->
       let {op=_;arg} = value in
       let* res = fold_expression f init arg in
       ok @@ res
    | E_Lt    {value;region=_}
      | E_Leq   {value;region=_}
      | E_Gt    {value;region=_}
      | E_Geq   {value;region=_}
      | E_Equal {value;region=_}
      | E_Neq   {value;region=_} ->
       bin_op value
    | E_Add   {value;region=_}
      | E_Sub   {value;region=_}
      | E_Mult  {value;region=_}
      | E_Div   {value;region=_}
      | E_Mod   {value;region=_} ->
       bin_op value
    | E_Neg   {value;region=_} ->
       let {op=_;arg} = value in
       let* res = fold_expression f init arg in
       ok @@ res
    | E_Int _ | E_Nat _ | E_Mutez _ -> ok @@ init
    | E_Cat {value;region=_} -> bin_op value
    | E_String _ -> ok init
    | E_Cons {value;region=_} -> bin_op value
    | E_List {value;region=_} ->
       bind_fold_list self init @@ pseq_to_list value.elements
    | E_Nil _ -> ok @@ init
    | E_App {value;region=_} ->
       let _, expr = value in
       (match expr with
          None -> ok @@ init
        | Some e ->
           bind_fold_ne_list self init @@ npseq_to_ne_list e.value.inside
       )
    | E_Ctor _ -> ok init
    | E_Record  {value;region=_} ->
       let aux init ({value;region=_} : (expr, expr) field reg) =
        match value with
        | Punned x -> self init x.pun
        | Complete {field_lhs ; field_rhs ; _} ->
          let* init = self init field_lhs in
          self init field_rhs
       in
       bind_fold_pseq aux init value.elements
    | E_Proj { value ; region = _ } ->
      self init value.record_or_tuple
    | E_Update  {value;region=_} ->
      let* init = self init value.structure in
      self init value.update
    | E_ModPath    {value;region=_} -> self init value.field
    | E_Var     _ -> ok init
    | E_Call    {value;region=_} ->
      let (lam, args) = value in
      let* res = self init lam in
      bind_fold_ne_list self res @@ npseq_to_ne_list args.value.inside
    | E_Bytes   _ -> ok @@ init
    | E_Tuple   {value;region=_} ->
      bind_fold_ne_list self init @@ npseq_to_ne_list value.inside
    | E_Par     {value;region=_} ->
      self init value.inside
    | E_Fun     {value;region=_} ->
      let ({ ret_type; return;_ }: fun_expr) = value in
      let* res = self init return in
      (match ret_type with
        Some (_, ty) -> self_type res ty
      | None ->    ok @@ res
      )
    | E_CodeInj {value;region=_} ->
      let {language=_;code;rbracket=_} = value in
      self init code
    | E_Set {value;region=_} ->
      bind_fold_list self init @@ pseq_to_list value.elements
    | E_SetMem {value;region=_} ->
      let {set;kwd_contains=_;element} = value in
      let* res = self init set in
      let* res = self res element in
      ok @@ res
    | E_MapLookup {value;region=_} ->
      let {keys;map;_} = value in
      let* init = bind_fold_list (fun init (x:expr brackets reg) -> self init x.value.inside) init (nseq_to_list keys) in
      self init map
    | E_Map {value;region=_} | E_BigMap {value;region=_} ->
      let aux init ({value;region=_}: _ reg) =
        let {key;arrow=_;value} = value in
        let* res = self init key in
        let* res = self res value in
        ok @@ res
      in
      bind_fold_list aux init @@ pseq_to_list value.elements
    | E_Block {value;region=_} ->
       let {block=b;kwd_with=_;expr} = value in
       let* res = fold_block f init b in
       let* res = self res expr in
       ok res
    | E_Attr (_,expr) -> self init expr
    | E_Verbatim _ -> ok init

  and fold_block f init ({value;region=_}: block reg) =
    let {enclosing=_;statements;terminator=_} = value in
    let* res = bind_fold_ne_list (fold_statement f) init @@ npseq_to_ne_list statements in
    ok @@ res

  and fold_statement : 'a folder -> 'a -> statement -> 'a monad = fun f init s  ->
    let self = fold_statement f in
    let self_expr = fold_expression f in
    let self_type = fold_type_expression f in
    let* init = f.s init s in
    let if_clause res = function
        ClauseInstr inst -> self res @@ S_Instr inst
      | ClauseBlock block -> fold_block f res block
    in
    match s with
    | S_Instr I_Cond {value;region=_} ->
      let {test;if_so;if_not;_} : _ conditional = value in
      let* res = self_expr init test in
      let* res = if_clause res if_so in
      let* res = match if_not with
        | Some (_,if_not) -> if_clause res if_not
        | None -> ok res
      in
      ok @@ res
    | S_Instr I_Case {value;region=_} ->
      let {kwd_case=_;expr;kwd_of=_;opening=_;lead_vbar=_;cases;closing=_} = value in
      let* res = self_expr init expr in
      let* res = matching_cases if_clause res cases in
      ok @@ res
    | S_Instr I_Assign {value;region=_} ->
      let {lhs; assign=_;rhs} = value in
      let* res = self_expr init lhs in
      let* res = self_expr res rhs in
      ok @@ res
    | S_Instr I_While  {value;region=_} ->
      let {kwd_while=_;cond;block} = value in
      let* res = self_expr init cond in
      let* res = fold_block f res block in
      ok @@ res
    | S_Instr I_For  {value;region=_} ->
      let {kwd_for=_;index=_;assign=_;init=i;kwd_to=_;bound;step;block} = value in
      let* res = self_expr init i in
      let* res = self_expr res bound in
      let* res = match step with
          Some (_,expr) -> self_expr res expr | None -> ok @@ res in
      let* res = fold_block f res block in
      ok @@ res
    | S_Instr I_ForIn (ForMap {value;region=_}) ->
      let {kwd_for=_;binding=_;kwd_in=_;kwd_map=_;collection;block} : for_map = value in
      let* res = self_expr init collection in
      let* res = fold_block f res block in
      ok @@ res
    | S_Instr I_ForIn (ForSetOrList {value;region=_}) ->
      let {kwd_for=_;var=_;kwd_in=_;for_kind=_;collection;block} : for_set_or_list = value in
      let* res = self_expr init collection in
      let* res = fold_block f res block in
      ok @@ res
    | S_Instr I_Call {value;region=_} ->
      let (expr, arguments) = value in
      let* res = self_expr init expr in
      let* res = bind_fold_ne_list self_expr res @@ npseq_to_ne_list arguments.value.inside in
      ok @@ res
    | S_Instr I_Skip _ -> ok @@ init
    | S_Instr I_Patch {value;region=_} ->
      let {kwd_patch=_;collection;kwd_with=_;patch_kind=_;patch} = value in
      let* res = self_expr init collection in
      let* res = self_expr res patch in
      ok @@ res
    | S_Instr I_Remove {value;region=_} ->
      let {kwd_remove=_;item;kwd_from=_;remove_kind=_;collection} = value in
      let* res = self_expr init item in
      let* res = self_expr res collection in
      ok @@ res
    | S_Decl x -> fold_declaration f init x
    | S_Attr (_,s) -> self init s
    | S_VarDecl {value ; region=_} ->
      let { kwd_var = _ ; pattern=_ ; type_params = _ ; var_type ; assign = _ ; init ; terminator = _} = value in
      let* res = match var_type with
        | Some (_,ty) -> self_type init ty
        | None -> ok init
      in
      let* res = self_expr res init in
      ok @@ res


  and matching_cases : type b.('a -> b -> _) -> 'a -> (b case_clause reg, vbar) Utils.nsepseq -> _ = fun self init value ->
    let case_clause self init ({value;region=_}: _ case_clause reg) =
      let {pattern=_;arrow=_;rhs} = value in
      self init rhs
    in
    bind_fold_npseq (case_clause self) init value

  and fold_declaration : 'a folder -> 'a -> declaration -> 'a monad =
    fun f init d ->
    let self_expr = fold_expression f in
    let self_type = fold_type_expression f in
    let self_module = fold_module f in
    let* init = f.d init d in
    match d with
    | D_Const {value;region=_} -> (
      let {kwd_const=_;pattern=_;type_params=_;const_type;equal=_;init=init_expr;terminator=_;} = value in
      let* res = self_expr init init_expr in
      match const_type with
      | Some (_, ty) -> self_type res ty
      | None ->    ok res
    )
    | D_Fun {value;region=_} -> (
      let {kwd_recursive=_;kwd_function=_;fun_name=_;type_params=_;parameters=_;ret_type;kwd_is=_;return;terminator=_} = value in
      let* res = self_expr init return in
      match ret_type with
      | Some (_, ty) -> self_type res ty
      | None ->    ok res
    )
    | D_Type {value;region=_} ->
       let {kwd_type=_;name=_;kwd_is=_;type_expr;params=_;terminator=_} = value in
       let* res = self_type init type_expr in
       ok @@ res
    | D_Module {value;region=_} ->
       let {kwd_module=_;name=_;kwd_is=_;enclosing=_;declarations;terminator=_} = value in
       let* res = self_module init declarations in
       ok @@ res
    | D_ModAlias {value;region=_} ->
       let {kwd_module=_;alias=_;kwd_is=_;mod_path=_;terminator=_} = value in
       ok @@ init
    | D_Directive _ -> ok init
    | D_Attr (_,x) -> fold_declaration f init x

  and fold_module : 'a folder -> 'a -> declarations -> 'a monad =
    fun f init decl ->
    let self = fold_declaration f in
    bind_fold_ne_list self init @@ decl

  type mapper = {
      e : expr -> (bool * expr) monad ;
      t : type_expr -> type_expr monad ;
      s : statement -> statement monad ;
      d : declaration -> declaration monad ;
    }

  let rec map_type_expression : mapper -> type_expr -> 'b monad = fun f t ->
    let self = map_type_expression f in
    let* t = f.t t in
    let return = ok in
    match t with
    | T_Attr (x,ty) ->
      let* ty = self ty in
      return @@ T_Attr (x,ty)
    | T_Cart {value = (ty,times,lst) ; region} ->
      let* ty = self ty in
      let* lst = bind_map_npseq self lst in
      return @@ T_Cart {value = (ty,times,lst) ; region}
    | T_Sum {value;region} ->
      let {lead_vbar;variants} = value in
      let aux (x : _ reg) =
        let {ctor_args;_} = x.value in
        match ctor_args with
          Some (c,t) ->
            let* t = self t in
            ok { x with value = { x.value with ctor_args = Some (c,t)}}
        | None -> ok x
      in
      let* variants = bind_map_npseq aux variants in
      let value : sum_type = { lead_vbar ; variants } in
      return @@ T_Sum {value ; region}
    | T_Record {value;region} ->
       let aux ({value;region} : field_decl reg) =
         match value.field_type with
         | Some (c,ty) ->
          let* ty = self ty in
          ok ({value = {value with field_type = Some (c,ty)} ; region} : field_decl reg)
         | None -> ok ({value ; region} : field_decl reg)
       in
       let* elements = bind_map_pseq aux value.elements in
       return @@ T_Record {value = {value with elements} ; region}
    | T_App {value;region} ->
       let (ty, tuple) = value in
       let* ty = self ty in
       let* inside = bind_map_npseq self tuple.value.inside in
       let tuple = {tuple with value = {tuple.value with inside}} in
       return @@ T_App {value = (ty,tuple) ; region}
    | T_Fun    {value;region} ->
      let (ty1, a, ty2) = value in
      let* ty1 = self ty1 in
      let* ty2 = self ty2 in
      return @@ T_Fun {value = (ty1,a,ty2) ; region}
    | T_Par    {value;region} ->
      let* inside = self value.inside in
      return @@ T_Par {value = {value with inside} ; region}
    | T_ModPath {value;region} ->
      let* field = self value.field in
      return @@ T_ModPath {value = {value with field} ; region}
    | T_Var    _ | T_Int    _ | T_String _ -> ok t

  let rec map_expression : mapper -> expr -> expr monad = fun f e  ->
    let self_type = map_type_expression f in
    let return = ok in
    let* (b, e) = f.e e in
    let self = if b then map_expression f else ok in
    let bin_op value =
      let {op;arg1;arg2} = value in
      let* arg1 = self arg1 in
      let* arg2 = self arg2 in
      ok @@ {op;arg1;arg2}
    in
    match e with
    | E_Case    {value;region} ->
       let {kwd_case=_;expr;kwd_of=_;opening=_;lead_vbar=_;cases;closing=_} = value in
       let* expr = self expr in
       let* cases = matching_cases self cases in
       let value = {value with expr;cases} in
       return @@ E_Case {value;region}
    | E_Cond    {value;region} ->
       let ({kwd_if=_;test;kwd_then=_;if_so;if_not} : _ conditional) = value in
       let* test = self test in
       let* if_so = self if_so in
       let* if_not =
        match if_not with
        | Some (kwd,expr) ->
          let* expr = self expr in
          ok @@ Some (kwd,expr)
        | None -> ok None
      in
       let value = {value with test;if_so;if_not} in
       return @@ E_Cond {value;region}
    | E_Typed {value;region} ->
       let expr, (comma, type_expr) = value.inside in
       let* expr = self expr in
       let* type_expr = self_type type_expr in
       let inside = expr, (comma, type_expr) in
       let value = {value with inside} in
       return @@ E_Typed {value;region}
    | E_Or  {value;region} ->
       let* value = bin_op value in
       return @@ E_Or {value;region}
    | E_And {value;region} ->
       let* value = bin_op value in
       return @@ E_And {value;region}
    | E_Not {value;region} ->
       let* arg = self value.arg in
       let value = {value with arg} in
       return @@ E_Not {value;region}
    | E_Lt    {value;region} ->
       let* value = bin_op value in
       return @@ E_Lt    {value;region}
    | E_Leq   {value;region} ->
       let* value = bin_op value in
       return @@ E_Leq   {value;region}
    | E_Gt    {value;region} ->
       let* value = bin_op value in
       return @@ E_Gt    {value;region}
    | E_Geq   {value;region} ->
       let* value = bin_op value in
       return @@ E_Geq   {value;region}
    | E_Equal {value;region} ->
       let* value = bin_op value in
       return @@ E_Equal {value;region}
    | E_Neq   {value;region} ->
       let* value = bin_op value in
       return @@ E_Neq   {value;region}
    | E_Add   {value;region} ->
       let* value = bin_op value in
       return @@ E_Add   {value;region}
    | E_Sub   {value;region} ->
       let* value = bin_op value in
       return @@ E_Sub   {value;region}
    | E_Mult  {value;region} ->
       let* value = bin_op value in
       return @@ E_Mult  {value;region}
    | E_Div   {value;region} ->
       let* value = bin_op value in
       return @@ E_Div   {value;region}
    | E_Mod   {value;region} ->
       let* value = bin_op value in
       return @@ E_Mod   {value;region}
    | E_Neg   {value;region} ->
       let* arg = self value.arg in
       let value = {value with arg} in
       return @@E_Neg   {value;region}
    | E_Int _ | E_Nat _ | E_Mutez _ as e -> return @@ e
    | E_Cat {value;region} ->
       let* value = bin_op value in
       return @@ E_Cat {value;region}
    | E_String _ | E_Verbatim _ as e -> return @@ e
    | E_Cons {value;region} ->
       let* value = bin_op value in
       return @@ E_Cons {value;region}
    | E_List {value;region} ->
       let* elements = bind_map_pseq self value.elements in
       let value = {value with elements} in
       return @@ E_List {value;region}
    | E_Nil _ as e -> return e
    | E_Ctor _ as e -> return e
    | E_App {value;region} ->
      let const, expr = value in
      let* expr = bind_map_option
        (fun (e : arguments) ->
          let* inside = bind_map_npseq self e.value.inside in
          ok {e with value = {e.value with inside}}
        )
        expr
      in
      let value = const,expr in
      return @@ E_App {value;region}
    | E_Record  {value;region} ->
       let aux (e : (expr,expr) field reg) =
        match e.value with
        | Punned x ->
          let* pun = self x.pun in
          ok {e with value = Punned {x with pun}}
        | Complete x ->
          let* field_lhs = self x.field_lhs in
          let* field_rhs = self x.field_rhs in
          ok {e with value = Complete {x with field_lhs ; field_rhs}}
       in
       let* elements = bind_map_pseq aux value.elements in
       let value = {value with elements} in
       return @@ E_Record {value;region}
    | E_Proj x ->
      let* record_or_tuple = self x.value.record_or_tuple in
      return @@ E_Proj {x with value = {x.value with record_or_tuple}}
    | E_Update  {value;region} ->
      let* update = self value.update in
      return @@ E_Update {value = {value with update} ; region}
    | E_ModPath {value;region} ->
       let* field = self value.field in
       let value = {value with field} in
       return @@ E_ModPath {value;region}
    | E_Var _ as e -> return e
    | E_Call {value;region} ->
       let (lam, args) = value in
       let* lam = self lam in
       let* inside = bind_map_npseq self args.value.inside in
       let args = {args with value = {args.value with inside}} in
       let value = (lam,args) in
       return @@ E_Call {value;region}
    | E_Bytes   _ as e -> return @@ e
    | E_Tuple   {value;region} ->
       let* inside = bind_map_npseq self value.inside in
       let value = {value with inside} in
       return @@ E_Tuple {value;region}
    | E_Par     {value;region} ->
       let* inside = self value.inside in
       let value = {value with inside} in
       return @@ E_Par {value;region}
    | E_Fun     {value;region} ->
       let ({kwd_function=_; parameters=_; type_params=_; ret_type; kwd_is=_; return=body}: fun_expr) = value in
       let* body = self body in
       let* ret_type = bind_map_option (fun (a,b) ->
                           let* b = self_type b in ok (a,b)) ret_type in
       let value = {value with return=body;ret_type} in
       return @@ E_Fun {value;region}
    | E_CodeInj {value;region} ->
       let* code = self value.code in
       let value = {value with code} in
       return @@ E_CodeInj {value;region}
    | E_Set {value;region} ->
       let* elements = bind_map_pseq self @@ value.elements in
       let value = {value with elements} in
       return @@ E_Set {value;region}
    | E_SetMem {value;region} ->
       let {set;kwd_contains;element} = value in
       let* set = self set in
       let* element = self element in
       let value = {set;kwd_contains;element} in
       return @@ E_SetMem {value;region}
    | E_MapLookup {value;region} ->
      let {map;keys = (hd,tl)} = value in
      let* hd =
        let* inside = self hd.value.inside in
        return {hd with value = {hd.value with inside}}
      in
      let* tl = bind_map_list
        (fun (x:expr brackets reg) ->
          let* inside = self x.value.inside in
          return {x with value = {x.value with inside}}
        )
        tl
      in
      let* map = self map in
      let value = {map;keys = (hd,tl)} in
      return @@ E_MapLookup {value;region}
    | E_Map {value;region} ->
      let aux (b: binding reg) =
        let {key;arrow;value} = b.value in
        let* key = self key in
        let* value  = self value in
        let value = {key;arrow;value} in
        ok @@ {b with value}
      in
      let* elements = bind_map_pseq aux value.elements in
      let value = {value with elements} in
      return @@ E_Map {value;region}
    | E_BigMap {value;region} ->
      let aux (b: binding reg) =
        let {key;arrow;value} = b.value in
        let* key = self key in
        let* value  = self value in
        let value = {key;arrow;value} in
        ok @@ {b with value}
      in
      let* elements = bind_map_pseq aux value.elements in
      let value = {value with elements} in
      return @@ E_BigMap {value;region}
    | E_Block {value;region} ->
       let {block;kwd_with;expr} = value in
       let* expr = self expr in
       let* block = map_block f block in
       let value = {block;kwd_with;expr} in
       return @@ E_Block {value;region}
    | E_Attr (x,a) ->
      let*a = self a in
      return @@ E_Attr (x,a)

  and map_block f (block: block reg) =
    let {enclosing=_;statements;terminator=_} = block.value in
    let* statements = bind_map_npseq (map_statement f) @@ statements in
    let value = {block.value with statements} in
    ok @@ {block with value}

  and map_statement : mapper -> statement -> statement monad = fun f s  ->
    let self_expr = map_expression f in
    let self_inst = map_instruction f in
    let self_type = map_type_expression f in
    let* s = f.s s in
    match s with
    | S_Instr inst -> let* inst = self_inst inst in ok @@ S_Instr inst
    | S_VarDecl {value;region} ->
       let* init = self_expr value.init in
       let* var_type = bind_map_option
        (fun (w, ty) ->
          let* ty = self_type ty in
          ok @@ (w,ty)
        )
        value.var_type
      in
       let value = {value with init;var_type} in
       ok @@ S_VarDecl {value;region}
    | S_Decl decl ->
      let* decl = map_declaration f decl in
      ok @@ S_Decl decl
    | S_Attr (a,s) ->
      let* s = map_statement f s in
      ok @@ S_Attr (a,s)

  and map_instruction = fun f i ->
    let self = map_instruction f in
    let self_expr = map_expression f in
    let if_clause = function
        ClauseInstr inst ->
         let* instr = self inst in
         ok @@ ClauseInstr instr
      | ClauseBlock block -> let* block = map_block f block in ok @@ ClauseBlock block
    in
    match i with
    | I_Cond        {value;region} ->
      let {kwd_if=_;test;kwd_then=_;if_so;if_not} : _ conditional = value in
      let* test  = self_expr test in
      let* if_so  = if_clause if_so in
      let* if_not = match if_not with
        | Some (a,if_not) ->
          let* if_not = if_clause if_not in
          ok @@ Some (a,if_not)
        | None -> ok None
      in
      let value : _ conditional = {value with test; if_so ; if_not } in
      ok @@ I_Cond {value;region}
    | I_Case   {value;region} ->
      let {kwd_case=_;expr;kwd_of=_;opening=_;lead_vbar=_;cases;closing=_} = value in
      let* expr = self_expr expr in
      let* cases = matching_cases if_clause cases in
      let value = {value with expr;cases} in
      ok @@ I_Case {value;region}
    | I_Assign      {value;region} ->
      let {lhs; assign;rhs} = value in
      let* lhs = self_expr lhs in
      let* rhs = self_expr rhs in
      let value = {lhs;assign;rhs} in
      ok @@ I_Assign {value;region}
    | I_While  {value;region} ->
      let {kwd_while;cond;block} = value in
      let* cond = self_expr cond in
      let* block = map_block f block in
      let value = {kwd_while;cond;block} in
      ok @@ I_While  {value;region}
    | I_For  {value;region} ->
      let* init = self_expr value.init in
      let* bound = self_expr value.bound in
      let* step = bind_map_option
        (fun (w,s) ->
          let* s = self_expr s in
          ok @@ (w,s)
        )
        value.step
      in
      let* block = map_block f value.block in
      let value = {value with init;bound;step;block} in
      ok @@ I_For {value;region}
    | I_ForIn ForMap {value;region} ->
      let* collection = self_expr value.collection in
      let* block = map_block f value.block in
      let value = {value with collection;block} in
      ok @@ I_ForIn (ForMap {value;region})
    | I_ForIn ForSetOrList {value;region} ->
      let* collection = self_expr value.collection in
      let* block = map_block f value.block in
      let value = {value with collection;block} in
      ok @@ I_ForIn (ForSetOrList {value;region})
    | I_Call {value;region} ->
      let (expr, arguments) = value in
      let* expr = self_expr expr in
      let* inside = bind_map_npseq self_expr arguments.value.inside in
      let arguments = {arguments with value = {arguments.value with inside}} in
      let value = (expr,arguments) in
      ok @@ I_Call {value;region}
    | I_Skip _ as i -> ok @@ i
    | I_Patch {value;region} ->
      let* collection = self_expr value.collection in
      let* patch = self_expr value.patch in
      let value = {value with collection ; patch } in
      ok @@ I_Patch {value;region}
    | I_Remove {value;region} ->
      let* item = self_expr value.item in
      let* collection = self_expr value.collection in
      let value = {value with item;collection} in
      ok @@ I_Remove {value;region}

  and matching_cases : type b. (b-> b monad) -> (b case_clause reg, vbar) Utils.nsepseq -> (b case_clause reg, vbar) Utils.nsepseq monad =
    fun self cases ->
      let case_clause self (case_clause: _ case_clause reg) =
        let {pattern=_;arrow=_;rhs} = case_clause.value in
        let* rhs = self rhs in
        let value = {case_clause.value with rhs} in
        ok @@ {case_clause with value}
      in
      let* cases = bind_map_npseq (case_clause self) cases in
      ok @@ cases

  and map_declaration : mapper -> declaration -> declaration monad =
    fun f d ->
    let self_expr = map_expression f in
    let self_type = map_type_expression f in
    let self_module = map_module f in
    let return = ok in
    let* d = f.d d in
    match d with
    | D_Attr (a,d) ->
      let* d = map_declaration f d in
      return @@ D_Attr (a,d)
    | D_Const {value;region} ->
       let* init = self_expr value.init in
       let* const_type = bind_map_option (fun (a,b) ->
                             let* b = self_type b in ok (a,b)) value.const_type in
       let value = {value with init;const_type} in
       return @@ D_Const {value;region}
    | D_Fun {value;region} ->
       (* let {kwd_recursive=_;kwd_function=_;fun_name=_;param=_;ret_type;kwd_is=_;return=expr;terminator=_;attributes=_} = value in *)
      let* return = self_expr value.return in
      let* ret_type = bind_map_option
      (fun (a,b) ->
        let* b = self_type b in
        ok (a,b)
      )
      value.ret_type
      in
      let value = {value with return;ret_type} in
      ok @@ D_Fun {value;region}
    | D_Type {value;region} ->
       let {kwd_type=_;name=_;kwd_is=_;type_expr;params=_;terminator=_} = value in
       let* type_expr = self_type type_expr in
       let value = {value with type_expr} in
       return @@ D_Type {value;region}
    | D_Module {value;region} ->
      let* declarations = self_module value.declarations in
      let value = {value with declarations} in
       return @@ D_Module {value;region}
    | D_ModAlias _ | D_Directive _ -> return d

  and map_module : mapper -> declarations -> declarations monad =
    fun f decl ->
    let self = map_declaration f in
    bind_map_ne_list self @@ decl

end
