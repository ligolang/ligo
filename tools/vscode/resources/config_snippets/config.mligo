// All these fields are optional. You can omit them
// and they would be asked if needed.
// The only required object here is 'config'.

let contract_env =
  { now           = "2020-01-01T00:00:00Z"
  ; balance       = 1tez
  ; amount        = 2tez
  ; self          = "KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b"
  ; source        = "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"
  ; sender        = "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"
  ; chain_id      = "NetXH12Aer3be93"
  ; level         = 10000
  ; voting_powers = Map.literal
      [ "tz1aZcxeRT4DDZZkYcU3vuBaaBRtnxyTmQRr", 40
      ; "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY", 60
      ]
  }

let config =
  { parameter    = "*parameter value*" : parameter_type
    // Note that the types of parameter and storage
    // should be monomorphized at the config resolution stage.
  ; storage      = "*storage value*" : storage_type
  ; program      = "*path to program*"
  ; module_name  = "*module name*"
  ; entrypoint   = "*entrypoint name*"
  ; log_dir      = "*log directory*"
  ; contract_env = contract_env
  }
