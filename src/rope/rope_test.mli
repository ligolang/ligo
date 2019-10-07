module A = struct
  (*
  open Rope

  val _ : unit
  *)
end

module B = struct
  (*
  open Rope_top_level_open

  (* type foo = S | NotCaptured *)
  (* let d = NotCaptured *)
  (* let s = NotCaptured *)

  val _ : unit
  *)
end
