#import "gitlab-pages/docs/modules/src/euro.mligo" "Euro"

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)