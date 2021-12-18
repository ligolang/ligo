type return = Done | Compileur_Error | Exception of exn
val return_result : return:return ref -> ?warn:bool -> ?output_file:string -> (unit -> (string*string,string*string) result) -> unit
