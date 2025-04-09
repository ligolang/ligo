module Euro = Gitlab_pages.Docs.Compiling.Src.Preprocessor.Euro

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)