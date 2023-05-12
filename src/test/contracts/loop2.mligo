let counter (n : nat) =
  let mut i = 0n in
  begin
    while i < n do
      i := i + 1n
    done;
    i
  end

let while_sum (n : nat) =
  let mut i = 0n in
  let mut r = 0n in
  begin
    while i < n do
      i := i + 1n;
      r := r + i
    done;
    r
  end

let empty_loop (n : nat) =
  let mut i = 0n in
  while (i < n) do done

let for_sum (n : nat) =
  let mut acc = 0 in
  begin
    for i = 1 upto int n do
        acc := acc + i
    done;
    acc
  end

let for_collection_list () =
  let mut acc = 0 in
  let mut str = "to" in
  let list = [1; 1; 1] in
  begin
    for x in list do
      acc := acc + x;
      str := str ^ "to"
    done;
    acc, str
  end

let for_collection_set () =
  let mut acc = 0 in
  let mut str = "to" in
  let list = Set.literal [1; 2; 3] in
  begin
    for x in list do
      acc := acc + x;
      str := str ^ "to"
    done;
    acc, str
  end

let for_collection_map () =
  let mut acc = 0 in
  let mut str = "" in
  let map = Map.literal [ "1", 1; "2", 2; "3", 3 ] in
  begin
    for k, v in map do
      acc := acc + v;
      str := str ^ k
    done;
    acc, str
  end
