(* Filtering the array [Sys.argv]. *)

let argv_list = Array.to_list Sys.argv |> List.tl (* Cannot fail *)

module SSet = Set.Make (String)

(* Valid options without an argument *)

let filter ~opt_with_arg ~opt_wo_arg : unit =
  let rec filter acc = function
      [] -> acc (* No more options. *)
    | "--"::more -> (* End of options: anonymous option. *)
       List.rev_append more ("--"::acc)
    | opt::more -> (* Named option *)
       if SSet.mem opt opt_wo_arg then
         (* Valid option without an argument. *)
         filter (opt::acc) more
       else (* Is it a valid option with an argument? *)
         if SSet.mem opt opt_with_arg then (* Yes *)
           match more with
             [] -> acc (* An error will be reported later. *)
           | arg::opts -> (* We keep the option and its argument. *)
              filter (arg::opt::acc) opts
         else (* Not a valid option: [opt] will be skipped. *)
           match more with
             [] -> acc (* [opt] has no argument and there are no more. *)
           | arg::opts -> (* Has [opt] an argument? *)
              if arg.[0] = '-' (* Cannot fail. *) then
                (* No argument: [arg] is actually the next option. *)
                filter acc more
              else (* [opt] has an argument [arg]: we skip it too. *)
                filter acc opts in
  let filtered =
    filter [] argv_list |> List.rev |> List.cons Sys.argv.(0) in
  let last =
    let patch i e = Sys.argv.(i) <- e; i+1 in
    List.fold_left patch 0 filtered
  in for i = last to Array.length Sys.argv - 1 do
       Sys.argv.(i) <- ""
     done
