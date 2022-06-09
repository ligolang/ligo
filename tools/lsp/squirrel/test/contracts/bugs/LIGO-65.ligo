type prod is (((int * (bool * nat))))

function cause_of_this_defect (const inp : list(unit)) : unit is
  case test of [
    x -> {
      const bar = unit
    } with bar
  ]

function test_expr (const inp: unit) : unit is inp
