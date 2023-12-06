(* This is gitlab-pages/docs/advanced/src/attributes-decorators/module-with-private.mligo *)
[@private] let stuff = 42
[@private] let g x = x * stuff
let f x = (g x) + 1