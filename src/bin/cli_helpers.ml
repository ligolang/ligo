open Trace

let toplevel x =
  match x with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Error ss -> (
      Format.printf "%a%!" Ligo.Display.error_pp (ss ())
    )

