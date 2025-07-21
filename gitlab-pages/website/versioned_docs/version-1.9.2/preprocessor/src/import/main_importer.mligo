module Euro = Gitlab_pages.Docs.Preprocessor.Src.Import.Euro

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)