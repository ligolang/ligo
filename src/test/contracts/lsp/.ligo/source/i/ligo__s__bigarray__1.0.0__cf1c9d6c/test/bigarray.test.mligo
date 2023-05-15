#import "../lib/bigarray.mligo" "Bigarray"

let test_fill =
  begin
    assert (Bigarray.fill 4 10 = [10;10;10;10]);
    assert (Bigarray.fill 4 "foo" = ["foo"; "foo"; "foo"; "foo"])
  end

let test_last_exn =
  let xs = [1; 2; 3] in
  let () = assert (Bigarray.last_exn xs = 3) in
  let xs = ["foo"; "bar"; "baz"] in
  assert (Bigarray.last_exn xs = "baz")

let test_reverse =
  let xs = [1; 2; 3] in
  let () = assert (Bigarray.reverse xs = [3; 2; 1]) in
  let xs = ["foo"; "bar"; "baz"] in
  assert (Bigarray.reverse xs = ["baz"; "bar"; "foo"])

let test_concat =
  let xs = [1; 2; 3] in
  let ys = [4; 5; 6] in
  let zs = Bigarray.concat xs ys in
  let () = assert (zs = [1; 2; 3; 4; 5; 6]) in
  let xs = ["foo"; "bar"; "baz"] in
  let ys = ["qux"; "quux"; "corge"] in
  let zs = Bigarray.concat xs ys in
  assert
    (zs = ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"])

let test_get_exn =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.get_exn xs 0 = 1);
    assert (Bigarray.get_exn xs 2 = 3);
    assert (Bigarray.get_exn xs 5 = 6);
    assert (Bigarray.get_exn ys 0 = "foo");
    assert (Bigarray.get_exn ys 2 = "baz");
    assert (Bigarray.get_exn ys 5 = "corge")
  end

let test_set_exn =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.set_exn xs 0 0 = [0; 2; 3; 4; 5; 6; 7]);
    assert (Bigarray.set_exn xs 2 33 = [1; 2; 33; 4; 5; 6; 7]);
    assert (Bigarray.set_exn xs 6 77 = [1; 2; 3; 4; 5; 6; 77]);
    assert
      (Bigarray.set_exn ys 0 "garply"
       = ["garply";
          "bar";
          "baz";
          "qux";
          "quux";
          "corge";
          "grault"]);
    assert
      (Bigarray.set_exn ys 2 "waldo"
       = ["foo";
          "bar";
          "waldo";
          "qux";
          "quux";
          "corge";
          "grault"]);
    assert
      (Bigarray.set_exn ys 6 "fred"
       = ["foo";
          "bar";
          "baz";
          "qux";
          "quux";
          "corge";
          "fred"])
  end

let test_insert_exn =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert
      (Bigarray.insert_exn xs 0 0 = [0; 1; 2; 3; 4; 5; 6; 7]);
    assert
      (Bigarray.insert_exn xs 2 3 = [1; 2; 3; 3; 4; 5; 6; 7]);
    assert
      (Bigarray.insert_exn xs 6 8 = [1; 2; 3; 4; 5; 6; 8; 7]);
    assert
      (Bigarray.insert_exn ys 0 "garply"
       = ["garply";
          "foo";
          "bar";
          "baz";
          "qux";
          "quux";
          "corge";
          "grault"]);
    assert
      (Bigarray.insert_exn ys 2 "waldo"
       = ["foo";
          "bar";
          "waldo";
          "baz";
          "qux";
          "quux";
          "corge";
          "grault"]);
    assert
      (Bigarray.insert_exn ys 6 "fred"
       = ["foo";
          "bar";
          "baz";
          "qux";
          "quux";
          "corge";
          "fred";
          "grault"])
  end

let test_remove_exn =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.remove_exn xs 0 = [2; 3; 4; 5; 6; 7]);
    assert (Bigarray.remove_exn xs 2 = [1; 2; 4; 5; 6; 7]);
    assert (Bigarray.remove_exn xs 6 = [1; 2; 3; 4; 5; 6]);
    assert
      (Bigarray.remove_exn ys 0
       = ["bar"; "baz"; "qux"; "quux"; "corge"; "grault"]);
    assert
      (Bigarray.remove_exn ys 2
       = ["foo"; "bar"; "qux"; "quux"; "corge"; "grault"]);
    assert
      (Bigarray.remove_exn ys 6
       = ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"])
  end

let test_drop_exn =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.drop_exn xs 0 = [1; 2; 3; 4; 5; 6; 7]);
    assert (Bigarray.drop_exn xs 2 = [3; 4; 5; 6; 7]);
    assert (Bigarray.drop_exn xs 6 = [7]);
    assert
      (Bigarray.drop_exn ys 0
       = ["foo";
          "bar";
          "baz";
          "qux";
          "quux";
          "corge";
          "grault"]);
    assert
      (Bigarray.drop_exn ys 2
       = ["baz"; "qux"; "quux"; "corge"; "grault"]);
    assert (Bigarray.drop_exn ys 6 = ["grault"])
  end

let test_take =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.take xs 0 = ([] : int list));
    assert (Bigarray.take xs 2 = [1; 2]);
    assert (Bigarray.take xs 6 = [1; 2; 3; 4; 5; 6]);
    assert (Bigarray.take ys 0 = ([] : string list));
    assert (Bigarray.take ys 2 = ["foo"; "bar"]);
    assert
      (Bigarray.take ys 6
       = ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"])
  end

let test_slice =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.slice xs 0 0 = ([] : int list));
    assert (Bigarray.slice xs 0 2 = [1; 2]);
    assert (Bigarray.slice xs 3 2 = [4; 5]);
    assert (Bigarray.slice xs 6 1 = [7]);
    assert (Bigarray.slice ys 0 0 = ([] : string list));
    assert (Bigarray.slice ys 0 2 = ["foo"; "bar"]);
    assert (Bigarray.slice ys 3 2 = ["qux"; "quux"]);
    assert (Bigarray.slice ys 6 1 = ["grault"])
  end

let test_split =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert
      (Bigarray.split xs 0
       = (([] : int list), [1; 2; 3; 4; 5; 6; 7]));
    assert (Bigarray.split xs 3 = ([1; 2; 3], [4; 5; 6; 7]));
    assert (Bigarray.split xs 5 = ([1; 2; 3; 4; 5], [6; 7]));
    assert
      (Bigarray.split ys 0
       = (([] : string list),
          ["foo";
           "bar";
           "baz";
           "qux";
           "quux";
           "corge";
           "grault"]));
    assert
      (Bigarray.split ys 3
       = (["foo"; "bar"; "baz"],
          ["qux"; "quux"; "corge"; "grault"]));
    assert
      (Bigarray.split ys 5
       = (["foo"; "bar"; "baz"; "qux"; "quux"],
          ["corge"; "grault"]))
  end

let test_rotate =
  let xs = [1; 2; 3; 4; 5; 6; 7] in
  let ys =
    ["foo"; "bar"; "baz"; "qux"; "quux"; "corge"; "grault"] in
  begin
    assert (Bigarray.rotate xs 0 = xs);
    assert (Bigarray.rotate xs 1 = [2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 1]);
    assert (Bigarray.rotate xs 4 = [5 ; 6 ; 7 ; 1; 2; 3; 4]);
    assert (Bigarray.rotate ys 1 = ["bar"; "baz"; "qux"; "quux"; "corge"; "grault"; "foo"]);
    assert (Bigarray.rotate ys 4 = ["quux"; "corge"; "grault"; "foo"; "bar"; "baz"; "qux"])
  end
