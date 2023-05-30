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
       , 'program
       , 'sig_expr
       , 'sig_entry)
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
        ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr, 'sig_expr) declaration_ -> 'declaration
    ; program_entry :
        ('program_entry, 'declaration, 'instruction) program_entry_ -> 'program_entry
    ; program : ('program, 'program_entry) program_ -> 'program
    ; sig_expr : ('sig_expr, 'sig_entry, 'ty_expr) sig_expr_ -> 'sig_expr
    ; sig_entry : ('sig_expr, 'sig_entry, 'ty_expr) sig_entry_ -> 'sig_entry
    }

  type idle_fold =
    ( expr
    , ty_expr
    , pattern
    , statement
    , block
    , mod_expr
    , instruction
    , declaration
    , program_entry
    , program
    , sig_expr
    , sig_entry)
    fold

  let idle : idle_fold =
    { expr = (fun x -> { fp = x })
    ; ty_expr = (fun x -> { fp = x })
    ; pattern = (fun x -> { fp = x })
    ; statement = (fun x -> { fp = x })
    ; block = (fun x -> { fp = x })
    ; mod_expr = (fun x -> { fp = x })
    ; instruction = (fun x -> { fp = x })
    ; declaration = (fun x -> { fp = x })
    ; program_entry = (fun x -> { fp = x })
    ; program = (fun x -> { fp = x })
    ; sig_expr = (fun x -> { fp = x })
    ; sig_entry = (fun x -> { fp = x })
    }


  let rec cata_expr
      : type e t p s b m i d pe prg sgt si. f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> expr -> e
    =
   fun ~f x ->
    map_expr_
      (cata_expr ~f)
      (cata_ty_expr ~f)
      (cata_pattern ~f)
      (cata_block ~f)
      (cata_mod_expr ~f)
      x.fp
    |> f.expr


  and cata_ty_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> ty_expr -> t
    =
   fun ~f x -> map_ty_expr_ (cata_ty_expr ~f) x.fp |> f.ty_expr


  and cata_pattern
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> pattern -> p
    =
   fun ~f x -> map_pattern_ (cata_pattern ~f) (cata_ty_expr ~f) x.fp |> f.pattern


  and cata_statement
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> statement -> s
    =
   fun ~f x ->
    map_statement_ (cata_statement ~f) (cata_instruction ~f) (cata_declaration ~f) x.fp
    |> f.statement


  and cata_block
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> block -> b
    =
   fun ~f x -> map_block_ (cata_block ~f) (cata_statement ~f) x.fp |> f.block


  and cata_mod_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> mod_expr -> m
    =
   fun ~f x -> map_mod_expr_ (cata_mod_expr ~f) (cata_program ~f) x.fp |> f.mod_expr


  and cata_instruction
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> instruction -> i
    =
   fun ~f x ->
    map_instruction_
      (cata_instruction ~f)
      (cata_expr ~f)
      (cata_pattern ~f)
      (cata_statement ~f)
      (cata_block ~f)
      x.fp
    |> f.instruction


  and cata_declaration
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> declaration -> d
    =
   fun ~f x ->
    map_declaration_
      (cata_declaration ~f)
      (cata_expr ~f)
      (cata_ty_expr ~f)
      (cata_pattern ~f)
      (cata_mod_expr ~f)
      (cata_sig_expr ~f)
      x.fp
    |> f.declaration


  and cata_program_entry
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> program_entry -> pe
    =
   fun ~f x ->
    map_program_entry_
      (cata_program_entry ~f)
      (cata_declaration ~f)
      (cata_instruction ~f)
      x.fp
    |> f.program_entry


  and cata_program
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> program -> prg
    =
   fun ~f x -> map_program_ (cata_program ~f) (cata_program_entry ~f) x.fp |> f.program


  and cata_sig_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> sig_expr -> sgt
    =
   fun ~f x -> map_sig_expr_ (cata_sig_expr ~f) (cata_sig_entry ~f) (cata_ty_expr ~f) x.fp |> f.sig_expr

  and cata_sig_entry
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) fold -> sig_entry -> si
    =
   fun ~f x -> map_sig_entry_ (cata_sig_expr ~f) (cata_sig_entry ~f) (cata_ty_expr ~f) x.fp |> f.sig_entry
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
       , 'program
       , 'sig_expr
       , 'sig_entry)
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
        'declaration -> ('declaration, 'expr, 'ty_expr, 'pattern, 'mod_expr, 'sig_expr) declaration_
    ; program_entry :
        'program_entry -> ('program_entry, 'declaration, 'instruction) program_entry_
    ; program : 'program -> ('program, 'program_entry) program_
    ; sig_expr : 'sig_expr -> ('sig_expr, 'sig_entry, 'ty_expr) sig_expr_
    ; sig_entry : 'sig_entry -> ('sig_expr, 'sig_entry, 'ty_expr) sig_entry_
    }

  let rec ana_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> e -> expr
    =
   fun ~f x ->
    { fp =
        f.expr x
        |> map_expr_
             (ana_expr ~f)
             (ana_ty_expr ~f)
             (ana_pattern ~f)
             (ana_block ~f)
             (ana_mod_expr ~f)
    }


  and ana_ty_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> t -> ty_expr
    =
   fun ~f x -> { fp = f.ty_expr x |> map_ty_expr_ (ana_ty_expr ~f) }


  and ana_pattern
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> p -> pattern
    =
   fun ~f x -> { fp = f.pattern x |> map_pattern_ (ana_pattern ~f) (ana_ty_expr ~f) }


  and ana_instruction
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> i -> instruction
    =
   fun ~f x ->
    { fp =
        f.instruction x
        |> map_instruction_
             (ana_instruction ~f)
             (ana_expr ~f)
             (ana_pattern ~f)
             (ana_statement ~f)
             (ana_block ~f)
    }


  and ana_statement
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> s -> statement
    =
   fun ~f x ->
    { fp =
        f.statement x
        |> map_statement_ (ana_statement ~f) (ana_instruction ~f) (ana_declaration ~f)
    }


  and ana_block
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> b -> block
    =
   fun ~f x -> { fp = f.block x |> map_block_ (ana_block ~f) (ana_statement ~f) }


  and ana_declaration
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> d -> declaration
    =
   fun ~f x ->
    { fp =
        f.declaration x
        |> map_declaration_
             (ana_declaration ~f)
             (ana_expr ~f)
             (ana_ty_expr ~f)
             (ana_pattern ~f)
             (ana_mod_expr ~f)
             (ana_sig_expr ~f)
    }


  and ana_mod_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> m -> mod_expr
    =
   fun ~f x -> { fp = f.mod_expr x |> map_mod_expr_ (ana_mod_expr ~f) (ana_program ~f) }


  and ana_program_entry
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> pe -> program_entry
    =
   fun ~f x ->
    { fp =
        f.program_entry x
        |> map_program_entry_
             (ana_program_entry ~f)
             (ana_declaration ~f)
             (ana_instruction ~f)
    }


  and ana_program
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> prg -> program
    =
   fun ~f x ->
    { fp = f.program x |> map_program_ (ana_program ~f) (ana_program_entry ~f) }


  and ana_sig_expr
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> sgt -> sig_expr
    =
   fun ~f x ->
    { fp = f.sig_expr x |> map_sig_expr_ (ana_sig_expr ~f) (ana_sig_entry ~f) (ana_ty_expr ~f) }

  and ana_sig_entry
      : type e t p s b m i d pe prg sgt si.
        f:(e, t, p, s, b, m, i, d, pe, prg, sgt, si) unfold -> si -> sig_entry
    =
   fun ~f x ->
    { fp = f.sig_entry x |> map_sig_entry_ (ana_sig_expr ~f) (ana_sig_entry ~f) (ana_ty_expr ~f) }
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
    ; declaration : (declaration, expr, ty_expr, pattern, mod_expr, sig_expr) declaration_ -> unit
    ; program_entry : (program_entry, declaration, instruction) program_entry_ -> unit
    ; program : (program, program_entry) program_ -> unit
    ; sig_expr : (sig_expr, sig_entry, ty_expr) sig_expr_ -> unit
    ; sig_entry : (sig_expr, sig_entry, ty_expr) sig_entry_ -> unit
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
    ; sig_expr = ignore
    ; sig_entry = ignore
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
      ; sig_expr =
          (fun x ->
             acc.sig_expr x;
             iter.sig_expr x)
      ; sig_entry =
          (fun x ->
             acc.sig_entry x;
             iter.sig_entry x)
      }
    in
    List.fold ~init:defaults ~f:aux iters


  let rec iter_expr ~(f : iter) (x : expr) : unit =
    f.expr x.fp;
    iter_expr_
      (iter_expr ~f)
      (iter_ty_expr ~f)
      (iter_pattern ~f)
      (iter_block ~f)
      (iter_mod_expr ~f)
      x.fp


  and iter_ty_expr ~(f : iter) (x : ty_expr) : unit =
    f.ty_expr x.fp;
    iter_ty_expr_ (iter_ty_expr ~f) x.fp


  and iter_pattern ~(f : iter) (x : pattern) : unit =
    f.pattern x.fp;
    iter_pattern_ (iter_pattern ~f) (iter_ty_expr ~f) x.fp


  and iter_instruction ~(f : iter) (x : instruction) : unit =
    f.instruction x.fp;
    iter_instruction_
      (iter_instruction ~f)
      (iter_expr ~f)
      (iter_pattern ~f)
      (iter_statement ~f)
      (iter_block ~f)
      x.fp


  and iter_statement ~(f : iter) (x : statement) : unit =
    f.statement x.fp;
    iter_statement_ (iter_statement ~f) (iter_instruction ~f) (iter_declaration ~f) x.fp


  and iter_block ~(f : iter) (x : block) : unit =
    f.block x.fp;
    iter_block_ (iter_block ~f) (iter_statement ~f) x.fp


  and iter_declaration ~(f : iter) (x : declaration) : unit =
    f.declaration x.fp;
    iter_declaration_
      (iter_declaration ~f)
      (iter_expr ~f)
      (iter_ty_expr ~f)
      (iter_pattern ~f)
      (iter_mod_expr ~f)
      (iter_sig_expr ~f)
      x.fp


  and iter_mod_expr ~(f : iter) (x : mod_expr) : unit =
    f.mod_expr x.fp;
    iter_mod_expr_ (iter_mod_expr ~f) (iter_program ~f) x.fp


  and iter_program_entry ~(f : iter) (x : program_entry) : unit =
    f.program_entry x.fp;
    iter_program_entry_
      (iter_program_entry ~f)
      (iter_declaration ~f)
      (iter_instruction ~f)
      x.fp


  and iter_program ~(f : iter) (x : program) : unit =
    f.program x.fp;
    iter_program_ (iter_program ~f) (iter_program_entry ~f) x.fp


  and iter_sig_expr ~(f : iter) (x : sig_expr) : unit =
    f.sig_expr x.fp;
    iter_sig_expr_ (iter_sig_expr ~f) (iter_sig_entry ~f) (iter_ty_expr ~f) x.fp


  and iter_sig_entry ~(f : iter) (x : sig_entry) : unit =
    f.sig_entry x.fp;
    iter_sig_entry_ (iter_sig_expr ~f) (iter_sig_entry ~f) (iter_ty_expr ~f) x.fp
end
