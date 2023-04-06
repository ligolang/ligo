open Types

module Catamorphism = struct
  (* f-algebras *)
  type ('expr
       , 'ty_expr
       , 'pattern
       , 'statement
       , 'block
       , 'mod_expr
       , 'instruction
       , 'declaration
       , 'program_entry
       , 'program)
       fold =
    { expr : ('expr, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_ -> 'expr
    ; ty_expr : 'ty_expr ty_expr_ -> 'ty_expr
    ; pattern : ('pattern, 'ty_expr) pattern_ -> 'pattern
    ; statement : ('statement, 'instruction, 'declaration) statement_ -> 'statement
    ; block : ('block, 'statement) block_ -> 'block
    ; mod_expr : ('mod_expr, 'program) mod_expr_ -> 'mod_expr
    ; instruction :
        ('instruction, 'expr, 'pattern, 'statement, 'block) instruction_ -> 'instruction
    ; declaration :
        ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_ -> 'declaration
    ; program_entry :
        ('program_entry, 'declaration, 'instruction) program_entry_ -> 'program_entry
    ; program : ('program, 'program_entry) program_ -> 'program
    }

  (* TODO: cleanup, write separated cata.;*)
  let rec cata_ty_expr : type t. f:(t ty_expr_ -> t) -> ty_expr -> t =
   fun ~f x -> map_ty_expr_ (cata_ty_expr ~f) x.fp |> f


  (* we could factorize cata_expr and cata_program ; but I feel like those function are exactly those
    we would like to generate from algebras someday, so I keep them as such *)
  let rec cata_expr
      : type e t p s b m i d pe prg. f:(e, t, p, s, b, m, i, d, pe, prg) fold -> expr -> e
    =
   fun ~f x ->
    let self = cata_expr ~f in
    let rec cata_ty_expr (x : ty_expr) : t = map_ty_expr_ cata_ty_expr x.fp |> f.ty_expr
    and cata_pattern (x : pattern) : p =
      map_pattern_ cata_pattern cata_ty_expr x.fp |> f.pattern
    and cata_instruction (x : instruction) : i =
      map_instruction_ cata_instruction self cata_pattern cata_statement cata_block x.fp
      |> f.instruction
    and cata_statement (x : statement) : s =
      map_statement_ cata_statement cata_instruction cata_declaration x.fp |> f.statement
    and cata_block (x : block) : b = map_block_ cata_block cata_statement x.fp |> f.block
    and cata_declaration (x : declaration) : d =
      map_declaration_ cata_declaration self cata_ty_expr cata_pattern cata_mod_expr x.fp
      |> f.declaration
    and cata_mod_expr (x : mod_expr) : m =
      map_mod_expr_ cata_mod_expr cata_program x.fp |> f.mod_expr
    and cata_program_entry (x : program_entry) : pe =
      map_program_entry_ cata_program_entry cata_declaration cata_instruction x.fp
      |> f.program_entry
    and cata_program (x : program) : prg =
      map_program_ cata_program cata_program_entry x.fp |> f.program
    in
    map_expr_ self cata_ty_expr cata_pattern cata_block cata_mod_expr x.fp |> f.expr


  let rec cata_program
      : type e t p s b m i d pe prg.
        f:(e, t, p, s, b, m, i, d, pe, prg) fold -> program -> prg
    =
   fun ~f x ->
    let self = cata_program ~f in
    let rec cata_ty_expr (x : ty_expr) : t = map_ty_expr_ cata_ty_expr x.fp |> f.ty_expr
    and cata_expr (x : expr) : e =
      map_expr_ cata_expr cata_ty_expr cata_pattern cata_block cata_mod_expr x.fp
      |> f.expr
    and cata_pattern (x : pattern) : p =
      map_pattern_ cata_pattern cata_ty_expr x.fp |> f.pattern
    and cata_instruction (x : instruction) : i =
      map_instruction_
        cata_instruction
        cata_expr
        cata_pattern
        cata_statement
        cata_block
        x.fp
      |> f.instruction
    and cata_statement (x : statement) : s =
      map_statement_ cata_statement cata_instruction cata_declaration x.fp |> f.statement
    and cata_block (x : block) : b = map_block_ cata_block cata_statement x.fp |> f.block
    and cata_declaration (x : declaration) : d =
      map_declaration_
        cata_declaration
        cata_expr
        cata_ty_expr
        cata_pattern
        cata_mod_expr
        x.fp
      |> f.declaration
    and cata_mod_expr (x : mod_expr) : m =
      map_mod_expr_ cata_mod_expr self x.fp |> f.mod_expr
    and cata_program_entry (x : program_entry) : pe =
      map_program_entry_ cata_program_entry cata_declaration cata_instruction x.fp
      |> f.program_entry
    in
    map_program_ self cata_program_entry x.fp |> f.program


  let rec cata_pattern
      : type e t p s b m i d pe prg.
        f:(e, t, p, s, b, m, i, d, pe, prg) fold -> pattern -> p
    =
   fun ~f x ->
    let self = cata_pattern ~f in
    let rec cata_ty_expr (x : ty_expr) : t =
      map_ty_expr_ cata_ty_expr x.fp |> f.ty_expr
    in
    map_pattern_ self cata_ty_expr x.fp |> f.pattern


  let rec cata_block
      : type e t p s b m i d pe prg.
        f:(e, t, p, s, b, m, i, d, pe, prg) fold -> block -> b
    =
   fun ~f x ->
    let self = cata_block ~f in
    let rec cata_ty_expr (x : ty_expr) : t = map_ty_expr_ cata_ty_expr x.fp |> f.ty_expr
    and cata_expr (x : expr) : e =
      map_expr_ cata_expr cata_ty_expr cata_pattern cata_block cata_mod_expr x.fp
      |> f.expr
    and cata_pattern (x : pattern) : p =
      map_pattern_ cata_pattern cata_ty_expr x.fp |> f.pattern
    and cata_block (x : block) : b = map_block_ cata_block cata_statement x.fp |> f.block
    and cata_mod_expr (x : mod_expr) : m =
      map_mod_expr_ cata_mod_expr cata_program x.fp |> f.mod_expr
    and cata_program_entry (x : program_entry) : pe =
      map_program_entry_ cata_program_entry cata_declaration cata_instruction x.fp
      |> f.program_entry
    and cata_program (x : program) : prg =
      map_program_ cata_program cata_program_entry x.fp |> f.program
    and cata_statement (x : statement) : s =
      map_statement_ cata_statement cata_instruction cata_declaration x.fp |> f.statement
    and cata_declaration (x : declaration) : d =
      map_declaration_
        cata_declaration
        cata_expr
        cata_ty_expr
        cata_pattern
        cata_mod_expr
        x.fp
      |> f.declaration
    and cata_instruction (x : instruction) : i =
      map_instruction_
        cata_instruction
        cata_expr
        cata_pattern
        cata_statement
        cata_block
        x.fp
      |> f.instruction
    in
    map_block_ self cata_statement x.fp |> f.block
end

module Anamorphism = struct
  (* f-coalgebras *)
  type ('expr
       , 'ty_expr
       , 'pattern
       , 'statement
       , 'block
       , 'mod_expr
       , 'instruction
       , 'declaration
       , 'program_entry
       , 'program)
       unfold =
    { expr : 'expr -> ('expr, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_
    ; ty_expr : 'ty_expr -> 'ty_expr ty_expr_
    ; pattern : 'pattern -> ('pattern, 'ty_expr) pattern_
    ; statement : 'statement -> ('statement, 'instruction, 'declaration) statement_
    ; block : 'block -> ('block, 'statement) block_
    ; mod_expr : 'mod_expr -> ('mod_expr, 'program) mod_expr_
    ; instruction :
        'instruction -> ('instruction, 'expr, 'pattern, 'statement, 'block) instruction_
    ; declaration :
        'declaration -> ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_
    ; program_entry :
        'program_entry -> ('program_entry, 'declaration, 'instruction) program_entry_
    ; program : 'program -> ('program, 'program_entry) program_
    }

  let rec ana_ty_expr : type t. f:(t -> t ty_expr_) -> t -> ty_expr =
   fun ~f x -> { fp = f x |> map_ty_expr_ (ana_ty_expr ~f) }


  let rec ana_expr
      : type e t p s b m i d pe prg.
        f:(e, t, p, s, b, m, i, d, pe, prg) unfold -> e -> expr
    =
   fun ~f x ->
    let self = ana_expr ~f in
    let rec ana_ty_expr (x : t) : ty_expr =
      { fp = f.ty_expr x |> map_ty_expr_ ana_ty_expr }
    and ana_pattern (x : p) : pattern =
      { fp = f.pattern x |> map_pattern_ ana_pattern ana_ty_expr }
    and ana_instruction (x : i) : instruction =
      { fp =
          f.instruction x
          |> map_instruction_ ana_instruction self ana_pattern ana_statement ana_block
      }
    and ana_statement (x : s) : statement =
      { fp = f.statement x |> map_statement_ ana_statement ana_instruction ana_declaration
      }
    and ana_block (x : b) : block =
      { fp = f.block x |> map_block_ ana_block ana_statement }
    and ana_declaration (x : d) : declaration =
      { fp =
          f.declaration x
          |> map_declaration_ ana_declaration self ana_ty_expr ana_pattern ana_mod_expr
      }
    and ana_mod_expr (x : m) =
      { fp = f.mod_expr x |> map_mod_expr_ ana_mod_expr ana_program }
    and ana_program_entry (x : pe) : program_entry =
      { fp =
          f.program_entry x
          |> map_program_entry_ ana_program_entry ana_declaration ana_instruction
      }
    and ana_program (x : prg) =
      { fp = f.program x |> map_program_ ana_program ana_program_entry }
    in
    { fp = f.expr x |> map_expr_ self ana_ty_expr ana_pattern ana_block ana_mod_expr }


  let rec ana_program
      : type e t p s b m i d pe prg.
        f:(e, t, p, s, b, m, i, d, pe, prg) unfold -> prg -> program
    =
   fun ~f x ->
    let self = ana_program ~f in
    let rec ana_ty_expr (x : t) : ty_expr =
      { fp = f.ty_expr x |> map_ty_expr_ ana_ty_expr }
    and ana_expr (x : e) : expr =
      { fp = f.expr x |> map_expr_ ana_expr ana_ty_expr ana_pattern ana_block ana_mod_expr
      }
    and ana_pattern (x : p) : pattern =
      { fp = f.pattern x |> map_pattern_ ana_pattern ana_ty_expr }
    and ana_instruction (x : i) : instruction =
      { fp =
          f.instruction x
          |> map_instruction_ ana_instruction ana_expr ana_pattern ana_statement ana_block
      }
    and ana_statement (x : s) : statement =
      { fp = f.statement x |> map_statement_ ana_statement ana_instruction ana_declaration
      }
    and ana_block (x : b) : block =
      { fp = f.block x |> map_block_ ana_block ana_statement }
    and ana_declaration (x : d) : declaration =
      { fp =
          f.declaration x
          |> map_declaration_
               ana_declaration
               ana_expr
               ana_ty_expr
               ana_pattern
               ana_mod_expr
      }
    and ana_mod_expr (x : m) = { fp = f.mod_expr x |> map_mod_expr_ ana_mod_expr self }
    and ana_program_entry (x : pe) : program_entry =
      { fp =
          f.program_entry x
          |> map_program_entry_ ana_program_entry ana_declaration ana_instruction
      }
    in
    { fp = f.program x |> map_program_ self ana_program_entry }


  let rec ana_pattern
      : type e t p s b m i d pe prg.
        f:(e, t, p, s, b, m, i, d, pe, prg) unfold -> p -> pattern
    =
   fun ~f x ->
    let self = ana_pattern ~f in
    let rec ana_ty_expr (x : t) : ty_expr =
      { fp = f.ty_expr x |> map_ty_expr_ ana_ty_expr }
    in
    { fp = f.pattern x |> map_pattern_ self ana_ty_expr }
end

module Iter = struct
  type iter =
    { expr : (expr, ty_expr, pattern, block, mod_expr) expression_ -> unit
    ; ty_expr : ty_expr ty_expr_ -> unit
    ; pattern : (pattern, ty_expr) pattern_ -> unit
    ; statement : (statement, instruction, declaration) statement_ -> unit
    ; block : (block, statement) block_ -> unit
    ; mod_expr : (mod_expr, program) mod_expr_ -> unit
    ; instruction : (instruction, expr, pattern, statement, block) instruction_ -> unit
    ; declaration : (declaration, expr, ty_expr, pattern, mod_expr) declaration_ -> unit
    ; program_entry : (program_entry, declaration, instruction) program_entry_ -> unit
    ; program : (program, program_entry) program_ -> unit
    }

  let defaults =
    { expr = ignore
    ; ty_expr = ignore
    ; pattern = ignore
    ; statement = ignore
    ; block = ignore
    ; mod_expr = ignore
    ; instruction = ignore
    ; declaration = ignore
    ; program_entry = ignore
    ; program = ignore
    }


  let combine_iteration : iter list -> iter =
   fun iters ->
    let aux acc iter =
      { expr =
          (fun x ->
            acc.expr x;
            iter.expr x)
      ; ty_expr =
          (fun x ->
            acc.ty_expr x;
            iter.ty_expr x)
      ; pattern =
          (fun x ->
            acc.pattern x;
            iter.pattern x)
      ; statement =
          (fun x ->
            acc.statement x;
            iter.statement x)
      ; block =
          (fun x ->
            acc.block x;
            iter.block x)
      ; mod_expr =
          (fun x ->
            acc.mod_expr x;
            iter.mod_expr x)
      ; instruction =
          (fun x ->
            acc.instruction x;
            iter.instruction x)
      ; declaration =
          (fun x ->
            acc.declaration x;
            iter.declaration x)
      ; program_entry =
          (fun x ->
            acc.program_entry x;
            iter.program_entry x)
      ; program =
          (fun x ->
            acc.program x;
            iter.program x)
      }
    in
    List.fold ~init:defaults ~f:aux iters


  let rec iter_expr ~(f : iter) (x : expr) : unit =
    let self = iter_expr ~f in
    let rec iter_ty_expr (x : ty_expr) : unit = f.ty_expr x.fp
    and iter_pattern (x : pattern) : unit =
      f.pattern x.fp;
      iter_pattern_ iter_pattern iter_ty_expr x.fp
    and iter_instruction (x : instruction) : unit =
      f.instruction x.fp;
      iter_instruction_ iter_instruction self iter_pattern iter_statement iter_block x.fp
    and iter_statement (x : statement) : unit =
      f.statement x.fp;
      iter_statement_ iter_statement iter_instruction iter_declaration x.fp
    and iter_block (x : block) : unit =
      f.block x.fp;
      iter_block_ iter_block iter_statement x.fp
    and iter_declaration (x : declaration) : unit =
      f.declaration x.fp;
      iter_declaration_ iter_declaration self iter_ty_expr iter_pattern iter_mod_expr x.fp
    and iter_mod_expr (x : mod_expr) : unit =
      f.mod_expr x.fp;
      iter_mod_expr_ iter_mod_expr iter_program x.fp
    and iter_program_entry (x : program_entry) : unit =
      f.program_entry x.fp;
      iter_program_entry_ iter_program_entry iter_declaration iter_instruction x.fp;
      iter_program_entry_ iter_program_entry iter_declaration iter_instruction x.fp
    and iter_program (x : program) : unit =
      f.program x.fp;
      iter_program_ iter_program iter_program_entry x.fp;
      iter_program_ iter_program iter_program_entry x.fp
    in
    f.expr x.fp;
    iter_expr_ self iter_ty_expr iter_pattern iter_block iter_mod_expr x.fp


  let rec iter_program ~(f : iter) (x : program) : unit =
    let rec iter_expr (x : expr) : unit =
      f.expr x.fp;
      iter_expr_ iter_expr iter_ty_expr iter_pattern iter_block iter_mod_expr x.fp
    and iter_ty_expr (x : ty_expr) : unit =
      f.ty_expr x.fp;
      iter_ty_expr_ iter_ty_expr x.fp
    and iter_pattern (x : pattern) : unit =
      f.pattern x.fp;
      iter_pattern_ iter_pattern iter_ty_expr x.fp
    and iter_instruction (x : instruction) : unit =
      f.instruction x.fp;
      iter_instruction_
        iter_instruction
        iter_expr
        iter_pattern
        iter_statement
        iter_block
        x.fp
    and iter_statement (x : statement) : unit =
      f.statement x.fp;
      iter_statement_ iter_statement iter_instruction iter_declaration x.fp
    and iter_block (x : block) : unit =
      f.block x.fp;
      iter_block_ iter_block iter_statement x.fp
    and iter_declaration (x : declaration) : unit =
      f.declaration x.fp;
      iter_declaration_
        iter_declaration
        iter_expr
        iter_ty_expr
        iter_pattern
        iter_mod_expr
        x.fp
    and iter_mod_expr (x : mod_expr) : unit =
      f.mod_expr x.fp;
      iter_mod_expr_ iter_mod_expr (iter_program ~f) x.fp
    and iter_program_entry (x : program_entry) : unit =
      f.program_entry x.fp;
      iter_program_entry_ iter_program_entry iter_declaration iter_instruction x.fp
    in
    f.program x.fp;
    iter_program_ (iter_program ~f) iter_program_entry x.fp


  let rec iter_pattern ~(f : iter) (x : pattern) : unit =
    let rec iter_ty_expr (x : ty_expr) : unit =
      f.ty_expr x.fp;
      iter_ty_expr_ iter_ty_expr x.fp
    in
    f.pattern x.fp;
    iter_pattern_ (iter_pattern ~f) iter_ty_expr x.fp
end
