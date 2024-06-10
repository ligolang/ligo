(* This module implements pattern matching anomaly detection as described in
   the paper "Warnings for pattern matching"
   Link: http://moscova.inria.fr/~maranget/papers/warn/warn.pdf  *)

open Core
open Ligo_prim
module AST = Ast_typed
module C = AST.Combinators
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
module LMap = Map.Make (Label)
module LSet = Set.Make (Label)

let empty_set = Set.empty (module Label)

type raise = (Errors.typer_error, Main_warnings.all) Trace.raise

let cons_label = Label.of_string "#CONS"
let nil_label = Label.of_string "#NIL"
let t_unit = AST.t_unit ()

let wild_binder ~loc =
  let var = Value_var.of_input_var ~loc "_" in
  Binder.make var None


type simple_pattern =
  | SP_Wildcard of AST.type_expression
  | SP_Constructor of Label.t * simple_pattern list * AST.type_expression

let nil_constructor ~loc ty = SP_Constructor (nil_label, [ SP_Wildcard (t_unit ~loc) ], ty)
let list_constructors = Set.of_list (module Label) [ cons_label; nil_label ]

let rec pp_simple_pattern ppf sp =
  match sp with
  | SP_Wildcard t -> Format.fprintf ppf "_ : %a" AST.PP.type_expression t
  | SP_Constructor (Label (c, _), ps, _) ->
    Format.fprintf
      ppf
      "%s (%s)"
      c
      (String.concat
         ~sep:", "
         (List.map ps ~f:(fun p -> Format.asprintf "%a" pp_simple_pattern p)))


let pp_simple_pattern_list ppf sps =
  List.iter sps ~f:(fun sp -> Format.fprintf ppf "%a, " pp_simple_pattern sp)


let get_variant_nested_type label (tsum : AST.row) =
  let label_map = tsum.fields in
  Map.find_exn label_map label


let rec destructure_type (t : AST.type_expression) =
  match t.type_content with
  | AST.T_record { fields; _ } ->
    Map.fold
      ~f:(fun ~key:_ ~data:elt_typ ts -> ts @ destructure_type elt_typ)
      fields
      ~init:[]
  | _ -> [ t ]


let rec to_simple_pattern (ty_pattern : _ AST.Pattern.t * AST.type_expression) =
  let pattern', ty = ty_pattern in
  let loc = Location.get_location pattern' in
  let pattern = Location.unwrap pattern' in
  match pattern with
  | P_unit -> [ SP_Wildcard ty ]
  | P_var _ when C.is_t_record ty ->
    let fields = Option.value_exn ~here:[%here] (C.get_record_fields ty) in
    let fields = List.map ~f:snd fields in
    let ps = List.init (List.length fields) ~f:(fun _ -> pattern') in
    let ps = List.zip_exn ps fields in
    let ps = List.map ps ~f:to_simple_pattern in
    List.concat ps
  | P_var _ -> [ SP_Wildcard ty ]
  | P_list (Cons (hd, tl)) ->
    let hd_ty = Option.value_exn ~here:[%here] (C.get_t_list ty) in
    let hd_tl = to_simple_pattern (hd, hd_ty) @ to_simple_pattern (tl, ty) in
    [ SP_Constructor (cons_label, hd_tl, ty) ]
  | P_list (List ps) ->
    let hd_ty = Option.value_exn ~here:[%here] (C.get_t_list ty) in
    List.fold_right
      ps
      ~init:[ nil_constructor ~loc ty ]
      ~f:(fun p acc ->
        let hd_tl = to_simple_pattern (p, hd_ty) @ acc in
        [ SP_Constructor (cons_label, hd_tl, hd_ty) ])
  | P_variant (c, p) ->
    let p_ty =
      get_variant_nested_type
        c
        (Option.value_exn ~here:[%here] (Option.map ~f:fst @@ C.get_t_sum ty))
    in
    [ SP_Constructor (c, to_simple_pattern (p, p_ty), ty) ]
  | P_tuple ps ->
    let row = Option.value_exn ~here:[%here] (C.get_t_record ty) in
    let ps =
      List.mapi ps ~f:(fun i p ->
          let row_elem =
            Option.value_exn
              ~here:[%here]
              (Map.find row.fields (Label.create (Int.to_string i)))
          in
          to_simple_pattern (p, row_elem))
    in
    List.concat ps
  | P_record lps ->
    let row = Option.value_exn ~here:[%here] (C.get_t_record ty) in
    let ps =
      List.map (Record.to_list lps) ~f:(fun (label, p) ->
          let row_elem = Option.value_exn ~here:[%here] (Map.find row.fields label) in
          to_simple_pattern (p, row_elem))
    in
    List.concat ps


let are_keys_numeric keys =
  List.for_all keys ~f:(fun l -> Option.is_some @@ int_of_string_opt @@ Label.to_string l)


let rec to_list_pattern ~(raise : raise) ~loc simple_pattern : _ AST.Pattern.t =
  match simple_pattern with
  | SP_Wildcard _ -> Location.wrap ~loc @@ AST.Pattern.P_var (wild_binder ~loc)
  | SP_Constructor (Label ("#NIL", _), _, _) ->
    Location.wrap ~loc @@ AST.Pattern.P_list (List [])
  | SP_Constructor (Label ("#CONS", _), sps, t) ->
    let rsps = List.rev sps in
    let tl = List.hd_exn rsps in
    let hd = List.rev (List.tl_exn rsps) in
    let hd = to_original_pattern ~raise ~loc hd (C.get_t_list_exn t) in
    let tl = to_list_pattern ~raise ~loc tl in
    Location.wrap ~loc @@ AST.Pattern.P_list (Cons (hd, tl))
  | SP_Constructor (Label (c, _), _, _) ->
    raise.error
    @@ Errors.corner_case (Format.sprintf "edge case: %s in to_list_pattern" c) loc


and to_original_pattern ~raise ~loc simple_patterns (ty : AST.type_expression) =
  let open AST.Pattern in
  match simple_patterns with
  | [] ->
    raise.error @@ Errors.corner_case "edge case: to_original_pattern empty patterns" loc
  | [ SP_Wildcard t ] when AST.is_t_unit t -> Location.wrap ~loc @@ P_unit
  | [ SP_Wildcard _ ] -> Location.wrap ~loc @@ P_var (wild_binder ~loc)
  | [ (SP_Constructor (Label ("#CONS", _), _, _) as simple_pattern) ]
  | [ (SP_Constructor (Label ("#NIL", _), _, _) as simple_pattern) ] ->
    to_list_pattern ~raise ~loc simple_pattern
  | [ SP_Constructor (c, sps, t) ] ->
    let t =
      get_variant_nested_type
        c
        (Option.value_exn ~here:[%here] (Option.map ~f:fst @@ C.get_t_sum t))
    in
    let ps = to_original_pattern ~raise ~loc sps t in
    Location.wrap ~loc @@ P_variant (c, ps)
  | _ ->
    (match ty.type_content with
    | AST.T_record { fields; _ } ->
      let kvs = Map.to_alist fields in
      let labels, tys = List.unzip kvs in
      let tys = List.map tys ~f:(fun ty -> ty) in
      let _, ps =
        List.fold_left tys ~init:(simple_patterns, []) ~f:(fun (sps, ps) t ->
            let n = List.length @@ destructure_type t in
            let sps, rest = List.split_n sps n in
            rest, ps @ [ to_original_pattern ~raise ~loc sps t ])
      in
      if are_keys_numeric labels
      then Location.wrap ~loc @@ P_tuple ps
      else Location.wrap ~loc @@ P_record (Record.of_list (List.zip_exn labels ps))
    | _ -> raise.error @@ Errors.corner_case "edge case: not a record/tuple" ty.location)


let[@warning "-32"] print_matrix matrix =
  let () = Format.printf "matrix: \n" in
  let () =
    List.iter matrix ~f:(fun row -> Format.printf "[%a]\n" pp_simple_pattern_list row)
  in
  Format.printf "\n"


let[@warning "-32"] print_vector vector =
  Format.printf "vector: \n%a\n" pp_simple_pattern_list vector


(* Specialize matrix [specialize_matrix c tys matrix]

    +-----------------------------+------------------------------+
    |               pi1           |      S (c, matrix)           |
    +-----------------------------+------------------------------+
    | c (r1, ... , ra)            | r1, ... , ra, pi2, ... , pin |
    | c'(r1, ... , ra) (c' != c)  | No Row                       |
    | _                           | _, ... , _, pi2, ... , pin   |
    +-----------------------------+------------------------------+

  In simple words look for constructor c in the 1st column of the matrix
  and explore the inner patterns.
  Ignore the constructors which are not c.
  If wildcard then add filler wildcards *)
let specialize_matrix c tys matrix =
  let specialize specialized row =
    match row with
    | SP_Constructor (cp, r1_a, _) :: p2_n when Label.equal c cp ->
      let row = r1_a @ p2_n in
      row :: specialized
    | SP_Constructor _ :: _ -> specialized
    | SP_Wildcard _ :: p2_n ->
      let wildcards = List.map tys ~f:(fun t -> SP_Wildcard t) in
      let row = wildcards @ p2_n in
      row :: specialized
    | [] -> specialized
  in
  List.fold_left matrix ~init:[] ~f:specialize


(* Specialize vector [specialize_vector c tys q1_n]

    Let
      vector = [q1, ... , qn]
      c : c(r1, ... , ra)

    S(c, (c(r1, ... , ra), q2, ... , qn)) = r1, ... , ra, q2, ... , qn

    S(c, (_, q1, ... , qn)) = _, .... , _, q2, ... , qn
                              <-a times->

    In simple words if the 1st element of [vector] is a constructor
    explore the inner patterns.
    If 1st element of [vector] is wildcard, add filler wildcards *)
let specialize_vector c tys q1_n =
  match q1_n with
  | SP_Constructor (cp, r1_a, _) :: q2_n when Label.equal c cp -> r1_a @ q2_n
  | SP_Wildcard _ :: q2_n ->
    let wildcards = List.map tys ~f:(fun t -> SP_Wildcard t) in
    wildcards @ q2_n
  | _ -> failwith "edge case: specialize_vector wrong constructor"


(* Default matrix [default_matrix matrix]

    +-----------------------------+------------------------------+
    |               pi1           |      D (matrix)              |
    +-----------------------------+------------------------------+
    | c (r1, ... , ra)            | No Row                       |
    | _                           | pi2, ... , pin               |
    +-----------------------------+------------------------------+

  In simple words ignore the rows in [matrix] which has constructors
  in the 1st column.
  If 1st of a row starts with a wildcard, explore the other columns *)
let default_matrix matrix =
  let default row dp =
    match row with
    | SP_Constructor _ :: _ -> dp
    | SP_Wildcard _ :: p2_n -> p2_n :: dp
    | [] -> dp
  in
  List.fold_right matrix ~init:[] ~f:default


let find_constuctor_arity c (t : AST.type_expression) =
  match (c : Label.t) with
  | Label ("#CONS", _) ->
    let t' = C.get_t_list_exn t in
    destructure_type t' @ [ t ]
  | Label ("#NIL", _) -> [ t ]
  | _ ->
    let te =
      get_variant_nested_type
        c
        (Option.value_exn ~here:[%here] (Option.map ~f:fst @@ C.get_t_sum t))
    in
    destructure_type te


let get_all_constructors (t : AST.type_expression) =
  if C.is_t_list t
  then list_constructors
  else (
    match C.get_t_sum t with
    | Some (tsum, _) ->
      let label_map = tsum.fields in
      let labels = Map.keys label_map in
      Set.of_list (module Label) labels
    | None -> empty_set)


let get_constructors_from_1st_col matrix =
  List.fold_left matrix ~init:empty_set ~f:(fun s row ->
      match row with
      | SP_Constructor (c, _, _) :: _ -> Set.add s c
      | SP_Wildcard _ :: _ -> s
      | [] -> s)


(* Algorithm Urec [algorithm_Urec matrix vector]

   [vector] is (v1, v2, ... , vn)

   Base case:
    if there are no columns in [matrix] (we check if some rows are present)
      if some rows are prent in [matrix]
      then false (meaning [vectors] matches some row of [matrix])
      else true ([matrix] is empty, [vector] is not matced by any row)

   Induction:
    if there are some columns present in the [matrix] there are 2 cases
    we need to take care of

    Case 1: v1 is a contructor pattern (C(r1, r2, ... , ra))
      From [matrix] we extract a
       specialized_matrix = [specialize_matrix C tys matrix]
      and we also
        specialize_vector = [specialize_vector C tys vector]

      Finally,
        Urec (matrix, vector) = Urec (specialize_matrix, specialize_vector)

    Case 2: v1 is a wildcard pattern
      Let Σ = { C1, C2, ... , Cz } set of constructors that appear at
      root of patterns in 1st column

      Now there are further 2 cases depending on whether Σ has all the
      constructors.

      (a.) If Σ has all the constructors
        Urec (matrix, vector) =
          for(k = 1 to z)
            specialized_matrix = [specialize_matrix Ck tys matrix]
            specialize_vector = [specialize_vector Ck tys vector]
            Urec (specialized_matrix, specialize_vector)
          (all the results are or-ed (||) )

      (b.) If Σ dos not have all the constructors
        default_matrix = [default_matrix matrix]

        Urec (matrix, vector) = Urec (default_matrix, (v2, ... , vn)) *)
let rec algorithm_Urec ~(raise : raise) ~loc matrix vector =
  if List.is_empty matrix
  then true
  else if List.for_all matrix ~f:List.is_empty && List.is_empty vector
  then false
  else (
    match vector with
    | SP_Constructor (c, _r1_n, t) :: _q2_n ->
      let a = find_constuctor_arity c t in
      let matrix = specialize_matrix c a matrix in
      let vector = specialize_vector c a vector in
      algorithm_Urec ~raise ~loc matrix vector
    | SP_Wildcard t :: q2_n ->
      let complete_signature = get_all_constructors t in
      let constructors = get_constructors_from_1st_col matrix in
      if (not (Set.is_empty complete_signature))
         && Set.equal complete_signature constructors
      then
        Set.fold
          complete_signature
          ~f:(fun b c ->
            let tys = find_constuctor_arity c t in
            b
            || algorithm_Urec
                 ~raise
                 ~loc
                 (specialize_matrix c tys matrix)
                 (specialize_vector c tys vector))
          ~init:false
      else algorithm_Urec ~raise ~loc (default_matrix matrix) q2_n
    | [] -> raise.error @@ Errors.corner_case "edge case: algorithm Urec" loc)


(* Algorithm I [algorithm_I matrix n ts]

   Base case:
    I (empty_matrix, 0) = Some []
    I (matrix, 0) = None

   Induction:
    Let Σ be set of constructors what appear at root of patterns in 1st column

    Case 1: If Σ has all the constructors
      for all constructors ck
        ak be the number of values ck holds
        I ([specialize_matrix ck tys matrix], ak + n - 1)
      If all the calls return None then
        I (matrix, n) = None
      else
        if one of the calls returns a pattern,
          I (S (ck, matrix), ak + n - 1) = (r1, ... , rak, p2, ... ,pn)
        then
          I (matrix, n) = Some (ck(r1, ... , rak), p2, ... , pn)

    Case 2: If Σ dos not have all the constructors

      calculate I ([default_matrix matrix], n - 1),
      if it return None then I (matrix, n) = None
      otherwise,
        I (D(matrix), n - 1) = (p2, ... , pn)

        if Σ is empty
          I (matrix, n) = (_, p2, ... , pn)
        else
          I (matrix, n) = (C(_, ... ,_), p2, ... , pn)
          Here C is a constructor that does not belong to Σ

          If there more constructors that do not belong to Σ,
          we can improve by returning all the patterns that can be formed
          using the extra constructors. *)
let rec algorithm_I ~(raise : raise) ~loc matrix n ts =
  if n = 0
  then
    if List.is_empty matrix
    then Some [ [] ]
    else if List.for_all matrix ~f:List.is_empty
    then None
    else raise.error @@ Errors.corner_case "edge case: algorithm Urec" loc
  else (
    let constructors = get_constructors_from_1st_col matrix in
    let t, ts = List.split_n ts 1 in
    let t = List.hd_exn t in
    let complete_signature = get_all_constructors t in
    if (not @@ Set.is_empty constructors) && Set.equal constructors complete_signature
    then
      Set.fold
        complete_signature
        ~f:(fun p ck ->
          if Option.is_some p
          then p
          else (
            let tys = find_constuctor_arity ck t in
            let ak = List.length tys in
            let matrix = specialize_matrix ck tys matrix in
            let ps = algorithm_I ~raise ~loc matrix (ak + n - 1) (tys @ ts) in
            match ps with
            | Some ps ->
              Some
                (List.map ps ~f:(fun ps ->
                     let xs, ps = List.split_n ps ak in
                     [ SP_Constructor (ck, xs, t) ] @ ps))
            | None -> None))
        ~init:None
    else (
      let dp = default_matrix matrix in
      let ps = algorithm_I ~raise ~loc dp (n - 1) ts in
      match ps with
      | Some ps ->
        if Set.is_empty constructors
        then (
          let ps = List.map ps ~f:(fun ps -> [ SP_Wildcard t ] @ ps) in
          Some ps)
        else (
          let missing_constructors = Set.diff complete_signature constructors in
          let cs =
            Set.fold
              missing_constructors
              ~f:(fun cs c ->
                let tys = find_constuctor_arity c t in
                let ps = List.map tys ~f:(fun t -> SP_Wildcard t) in
                let c = SP_Constructor (c, ps, t) in
                c :: cs)
              ~init:[]
          in
          Some
            (List.fold_left cs ~init:[] ~f:(fun new_ps c ->
                 let ps = List.map ps ~f:(fun p -> [ c ] @ p) in
                 ps @ new_ps)))
      | None -> None))


(* Missing case analysis uses [algorithm_I matrix n] to find out the
   actual missing pattern(s)

   [n] is the number of parts a pattern has

    I([[p11, p12, ... , p1n]
       [p21, p22, ... , p2n]
        ...
       [pm1, pm2, ... , pmn]], n) *)
let missing_case_analysis ~raise ~loc matrix t =
  let ts = destructure_type t in
  match algorithm_I ~raise ~loc matrix (List.length ts) ts with
  | Some sps ->
    let ps = List.map sps ~f:(fun sp -> to_original_pattern ~raise ~loc sp t) in
    Some ps
  | None -> None


(* Redundant case analysis uses [algorithm_Urec] to check if any row of the
   matrix is redundant.
   A row in matrix P (pi) is redundant if
    Urec([[p1]
          [p2]
          ...
          [p(i-1)], pi) is false *)
let redundant_case_analysis ~raise ~loc matrix =
  let redundant, case, _ =
    List.fold_left
      matrix
      ~init:(false, 0, [])
      ~f:(fun (redundant_case_found, case, matrix) vector ->
        if redundant_case_found
        then true, case, []
        else if List.is_empty matrix
        then false, case + 1, [ vector ]
        else (
          let redundant_case_found = not @@ algorithm_Urec matrix ~raise ~loc vector in
          redundant_case_found, case + 1, matrix @ [ vector ]))
  in
  redundant, case


(* Pattern matching anomalies are of 2 kinds
   1. Missing case(s)
   2. Redundant case(s)
   To detect these anomalies we use [algorithm_Urec] & to find the actual
   missing pattern(s) we use [algorithm_I].

   In simple words [algorithm_Urec matrix vector] tells us whether [vector]
   is a useful pattern in the context of [matrix], if the result is true
   that means [vector] is a useful pattern.

   The flow goes as follows:
   a. We start by converting [eqs] (type-checked ligo patterns) into
   simple_pattern matrix.
   b. Using the matchee type [t] we create a vector of wildcards.
   c. Calculate [algorithm_Urec matrix vector] [Urec(P, (_,..,_))], if the
   result is true that means there are missing cases in [eqs].
   d. To find out the actual missing patterns we use [algorithm_I] which
   give you the missing pattern (simple_pattern), we need to convert the
   missing pattern to the original pattern representation
   using [to_original_pattern].
   e. If there are no missing cases we check for redundant cases. *)
let check_anomalies
    ~(raise : raise)
    ~syntax
    ~loc
    (eqs : (AST.type_expression AST.Pattern.t * AST.type_expression) list)
    t
  =
  let matrix = List.map eqs ~f:to_simple_pattern in
  match missing_case_analysis ~raise ~loc matrix t with
  | Some missing_cases ->
    raise.error @@ Errors.pattern_missing_cases syntax missing_cases loc
  | None ->
    let redundant, case = redundant_case_analysis ~raise ~loc matrix in
    if redundant
    then (
      let p, _ = List.nth_exn eqs (case - 1) in
      let loc = Location.get_location p in
      raise.error @@ Errors.pattern_redundant_case loc)
    else ()
