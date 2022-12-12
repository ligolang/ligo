module Path = Command.Private.Path

module For_unix = Command.Private.For_unix (struct
  module Signal = Signal

  module Thread = struct
    include Caml_threads.Thread

    let create ~on_uncaught_exn:_ f arg = create f arg
  end

  module Time = struct
    include Time

    let sexp_of_t = Obj.magic Fn.id
  end

  module Unix = struct
    include Ligo_unix

    type env =
      [ `Extend of (string * string) list
      | `Override of (string * string option) list
      | `Replace of (string * string) list
      | `Replace_raw of string list
      ]

    module File_descr = struct
      type t = int
    end

    module Exit = struct
      type error = [ `Exit_non_zero of Int.t ]
      type t = (unit, error) _result
    end

    module Exit_or_signal = struct
      type error =
        [ `Exit_non_zero of file_perm
        | `Signal of Signal.t
        ]

      type t = (unit, error) Result.t
    end

    module Process_info = struct
      type t =
        { pid : Pid.t
        ; stdin : file_perm
        ; stdout : file_perm
        ; stderr : file_perm
        }
    end

    let getpid = Obj.magic Fn.id

    type wait_on =
      [ `Any
      | `Group of Core.Pid.t
      | `My_group
      | `Pid of Core.Pid.t
      ]

    let unsetenv = Obj.magic 0
    let exec = Obj.magic 0
    let unsafe_getenv = Sys.getenv
    let close = Obj.magic Fn.id
    let close_process_in = Obj.magic Fn.id
    let putenv = Obj.magic Fn.id
    let in_channel_of_descr = Obj.magic Fn.id
    let create_process_env = Obj.magic Fn.id
    let wait = Obj.magic Fn.id
  end

  module Version_util = Version_util
end)

let run = For_unix.run
let shape = For_unix.shape

module Deprecated = struct
  let run = For_unix.deprecated_run
end

module Shape = struct
  let help_text = For_unix.help_for_shape
end
