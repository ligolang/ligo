let contract_env =
  { now           = "2020-01-01T00:00:00Z"
  ; balance       = 1tez
  ; amount        = 2tez
  ; self          = "KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b"
  ; source        = "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"
  }

let config =
  { parameter    = "some_param"
  ; module_name  = "default"
  ; log_dir      = "tmp/contract.log"
  ; contract_env = contract_env
  }
