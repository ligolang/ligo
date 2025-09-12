#import "gitlab-pages/docs/compiling/src/preprocessor/euro.mligo" "Euro"

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)