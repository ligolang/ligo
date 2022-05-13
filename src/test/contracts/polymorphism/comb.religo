type dup ('a) = ('a, 'a);

let diag = (type a, x : a) => (x, x);

let rec rev = (type a, (xs, acc) : (list (a), list (a))) : list (a) =>
  switch xs {
  | [] => acc
  | [x,... xs] => rev (xs, [x,... acc])
  };

let rev = (type a, xs : list (a)) : list (a) => rev (xs, ([] : list (a)));

let rec zip = (type (a,b), (xs, ys, acc) : (list (a), list (b), list ((a, b)))) : list ((a, b)) =>
  switch (xs) {
  | [] =>
       switch (ys) {
       | [] => acc
       | _  => failwith ("oops")
       }
  | [x, ...xs] =>
        switch (ys) {
        | [] => failwith ("oops")
        | [y, ...ys] => zip (xs, ys, [(x, y), ...acc])
        }
  };

let zip = (type (a,b), xs : list (a)) => (ys : list (b)) : list ((a, b)) => rev (zip (xs, ys, ([] : list ((a, b)))));

let self_zip = (type tau, xs : list (tau)) : list ((tau, tau)) =>
  let (xs, ys) = diag (xs);
  (zip (xs))(ys);

let v : list ((string, string)) = self_zip (["a","b"]);
let w : list ((int, nat)) = (zip ([1,2,3]))([4n,5n,6n]);
