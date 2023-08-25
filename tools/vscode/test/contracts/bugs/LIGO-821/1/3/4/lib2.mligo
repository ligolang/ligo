#import "ligo-breathalyzer/lib/lib.mligo" "Breath"

let one_plus_one_is_two =
  Breath.Model.case
    "_"
    "1 + 1 should be equal to 2"
    (fun (_ : Breath.Logger.level) ->
      let expected = 2 in
      let computed = 1 + 1 in
      Breath.Assert.is_true "should be equal" (expected = computed))

let () =
  Breath.Model.run_suites
    Trace
    [Breath.Model.suite "A simple sum" [one_plus_one_is_two]]

[@entry]
let main (_ : unit) (_ : unit) : operation list * unit = [], ()
