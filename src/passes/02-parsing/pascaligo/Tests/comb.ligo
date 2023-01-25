type dup(a) is a * a

function diag<a>(const x : a) : dup(a) is (x, x)

function rev<a>(const xs : list (a)) : list (a) is {
  var zs := (nil : list (a));
  for x in list xs {
    zs := x # zs;
  };
} with zs

function zip<a,b>(const xs : list(a); var ys : list(b)) : list(a * b) is {
  var zs := (nil : list(a * b));
  for x in list xs {
    var t := case ys of [
      | nil -> (failwith("error") : b * list(b))
      | (y # ys) -> (y, ys)
    ];
    zs := ((x, t.0) # zs);
    ys := t.1;
  };
  if List.length(ys) > 0n then failwith("error") ;
  zs := rev(zs);
} with zs

function self_zip<a>(const xs : list(a)) : list(a * a) is {
  const (xs, ys) = diag(xs);
} with zip(xs, ys)

const v = self_zip(list ["a";"b"])
const w = zip(list [1;2;3], list [4n;5n;6n])
