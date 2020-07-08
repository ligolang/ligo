open Simple_utils.Trace

let fold_map__list continue state v =
  let aux = fun acc elt ->
    let%bind (state , lst') = acc in
    let%bind (state , elt') = continue state elt in
    ok (state , elt' :: lst') in
  let%bind (state, l) = List.fold_left aux (ok (state, [])) v in
  (* fold_left with a list accumulator will produce the results in
     reverse order, so we apply List.rev to put them back in the right
     order. *)
  ok (state, List.rev l)


let fold_map__option continue state v =
  match v with
    Some x -> continue state x
  | None -> ok None

let make__list f l =
  List.fold_right
    (fun elt acc -> match acc, f elt with
         Some acc, Some x -> Some (x :: acc)
       | _ -> None)
    l (Some [])
