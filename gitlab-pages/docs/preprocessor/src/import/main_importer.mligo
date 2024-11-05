#import "gitlab-pages/docs/preprocessor/src/import/euro.mligo" "Euro"

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)