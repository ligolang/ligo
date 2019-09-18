open Trace

let toplevel x =
  match x with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Error ss -> (
      Format.printf "%a%!" Main.Display.error_pp (ss ())
    )

