open Lsp_helpers
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common

type selection_range_test =
  { file_path : string
  ; positions : Position.t list
  }

let run_selection_range_test { file_path; positions } : unit =
  let result, _diagnostics =
    test_run_session
    @@
    let open Handler.Let_syntax in
    let%bind uri = open_file @@ normalize_path file_path in
    Requests.on_req_selection_range uri positions
  in
  Format.printf
    "%a"
    (Helpers_pretty.pp_with_yojson [%yojson_of: SelectionRange.t list])
    result


let%expect_test "CameLIGO selection range" =
  let tests =
    [ { file_path = "contracts/lsp/selection_range.mligo"
      ; positions =
          [ (* comment *)
            { line = 0; character = 0 }
          ; { line = 0; character = 5 }
          ; (* module alias *)
            { line = 1; character = 0 }
          ; { line = 1; character = 10 }
          ; (* module *)
            { line = 4; character = 0 }
          ; { line = 6; character = 0 }
          ; (* entrypoint attribute *)
            { line = 8; character = 2 }
          ; { line = 8; character = 7 }
          ; (* type: sum *)
            { line = 12; character = 0 }
          ; { line = 12; character = 8 }
          ; (* variant *)
            { line = 13; character = 2 }
          ; { line = 13; character = 7 }
          ; (* function *)
            { line = 18; character = 0 }
          ; { line = 18; character = 8 }
          ; { line = 19; character = 12 }
          ; { line = 21; character = 38 }
          ; (* type: record *)
            { line = 25; character = 0 }
          ; { line = 25; character = 12 }
          ; { line = 26; character = 15 }
          ; (* value *)
            { line = 31; character = 0 }
          ; { line = 31; character = 11 }
          ; { line = 31; character = 18 }
          ; { line = 31; character = 31 }
          ; (* out of bounds *)
            { line = 100; character = 100 }
          ]
      }
    ; { file_path = "contracts/lsp/missing_a_lot.mligo"
      ; positions = [ { line = 0; character = 0 } ]
      }
    ]
  in
  run_multiple_tests tests ~test_runner:run_selection_range_test;
  [%expect
    {|
    [[
       {
         "parent": {
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 0 }
           }
         },
         "range": {
           "end": { "character": 8, "line": 0 },
           "start": { "character": 0, "line": 0 }
         }
       },
       {
         "parent": {
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 0 }
           }
         },
         "range": {
           "end": { "character": 8, "line": 0 },
           "start": { "character": 0, "line": 0 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 23, "line": 1 },
               "start": { "character": 0, "line": 0 }
             }
           },
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 1 }
           }
         },
         "range": {
           "end": { "character": 6, "line": 1 },
           "start": { "character": 0, "line": 1 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 23, "line": 1 },
               "start": { "character": 0, "line": 0 }
             }
           },
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 1 }
           }
         },
         "range": {
           "end": { "character": 11, "line": 1 },
           "start": { "character": 7, "line": 1 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 3, "line": 9 },
               "start": { "character": 0, "line": 3 }
             }
           },
           "range": {
             "end": { "character": 3, "line": 9 },
             "start": { "character": 0, "line": 4 }
           }
         },
         "range": {
           "end": { "character": 6, "line": 4 },
           "start": { "character": 0, "line": 4 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 3, "line": 9 },
               "start": { "character": 0, "line": 3 }
             }
           },
           "range": {
             "end": { "character": 3, "line": 9 },
             "start": { "character": 0, "line": 4 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 9 },
           "start": { "character": 11, "line": 4 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "range": {
                     "end": { "character": 3, "line": 9 },
                     "start": { "character": 0, "line": 3 }
                   }
                 },
                 "range": {
                   "end": { "character": 3, "line": 9 },
                   "start": { "character": 0, "line": 4 }
                 }
               },
               "range": {
                 "end": { "character": 3, "line": 9 },
                 "start": { "character": 11, "line": 4 }
               }
             },
             "range": {
               "end": { "character": 55, "line": 8 },
               "start": { "character": 2, "line": 7 }
             }
           },
           "range": {
             "end": { "character": 55, "line": 8 },
             "start": { "character": 2, "line": 8 }
           }
         },
         "range": {
           "end": { "character": 10, "line": 8 },
           "start": { "character": 2, "line": 8 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "range": {
                     "end": { "character": 3, "line": 9 },
                     "start": { "character": 0, "line": 3 }
                   }
                 },
                 "range": {
                   "end": { "character": 3, "line": 9 },
                   "start": { "character": 0, "line": 4 }
                 }
               },
               "range": {
                 "end": { "character": 3, "line": 9 },
                 "start": { "character": 11, "line": 4 }
               }
             },
             "range": {
               "end": { "character": 55, "line": 8 },
               "start": { "character": 2, "line": 7 }
             }
           },
           "range": {
             "end": { "character": 55, "line": 8 },
             "start": { "character": 2, "line": 8 }
           }
         },
         "range": {
           "end": { "character": 10, "line": 8 },
           "start": { "character": 2, "line": 8 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 15, "line": 15 },
               "start": { "character": 0, "line": 11 }
             }
           },
           "range": {
             "end": { "character": 15, "line": 15 },
             "start": { "character": 0, "line": 12 }
           }
         },
         "range": {
           "end": { "character": 4, "line": 12 },
           "start": { "character": 0, "line": 12 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 15, "line": 15 },
               "start": { "character": 0, "line": 11 }
             }
           },
           "range": {
             "end": { "character": 15, "line": 15 },
             "start": { "character": 0, "line": 12 }
           }
         },
         "range": {
           "end": { "character": 13, "line": 12 },
           "start": { "character": 5, "line": 12 }
         }
       },
       {
         "parent": {
           "range": {
             "end": { "character": 15, "line": 15 },
             "start": { "character": 0, "line": 11 }
           }
         },
         "range": {
           "end": { "character": 15, "line": 15 },
           "start": { "character": 0, "line": 12 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "range": {
                   "end": { "character": 15, "line": 15 },
                   "start": { "character": 0, "line": 11 }
                 }
               },
               "range": {
                 "end": { "character": 15, "line": 15 },
                 "start": { "character": 0, "line": 12 }
               }
             },
             "range": {
               "end": { "character": 15, "line": 15 },
               "start": { "character": 4, "line": 13 }
             }
           },
           "range": {
             "end": { "character": 21, "line": 13 },
             "start": { "character": 4, "line": 13 }
           }
         },
         "range": {
           "end": { "character": 14, "line": 13 },
           "start": { "character": 4, "line": 13 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 20, "line": 22 },
               "start": { "character": 0, "line": 17 }
             }
           },
           "range": {
             "end": { "character": 20, "line": 22 },
             "start": { "character": 0, "line": 18 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 18 },
           "start": { "character": 0, "line": 18 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 20, "line": 22 },
               "start": { "character": 0, "line": 17 }
             }
           },
           "range": {
             "end": { "character": 20, "line": 22 },
             "start": { "character": 0, "line": 18 }
           }
         },
         "range": {
           "end": { "character": 12, "line": 18 },
           "start": { "character": 4, "line": 18 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "range": {
                 "end": { "character": 20, "line": 22 },
                 "start": { "character": 0, "line": 17 }
               }
             },
             "range": {
               "end": { "character": 20, "line": 22 },
               "start": { "character": 0, "line": 18 }
             }
           },
           "range": {
             "end": { "character": 20, "line": 22 },
             "start": { "character": 2, "line": 19 }
           }
         },
         "range": {
           "end": { "character": 13, "line": 19 },
           "start": { "character": 8, "line": 19 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "parent": {
                     "parent": {
                       "range": {
                         "end": { "character": 20, "line": 22 },
                         "start": { "character": 0, "line": 17 }
                       }
                     },
                     "range": {
                       "end": { "character": 20, "line": 22 },
                       "start": { "character": 0, "line": 18 }
                     }
                   },
                   "range": {
                     "end": { "character": 20, "line": 22 },
                     "start": { "character": 2, "line": 19 }
                   }
                 },
                 "range": {
                   "end": { "character": 20, "line": 22 },
                   "start": { "character": 4, "line": 20 }
                 }
               },
               "range": {
                 "end": { "character": 43, "line": 21 },
                 "start": { "character": 4, "line": 20 }
               }
             },
             "range": {
               "end": { "character": 43, "line": 21 },
               "start": { "character": 23, "line": 21 }
             }
           },
           "range": {
             "end": { "character": 39, "line": 21 },
             "start": { "character": 37, "line": 21 }
           }
         },
         "range": {
           "end": { "character": 20, "line": 22 },
           "start": { "character": 2, "line": 21 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 3, "line": 28 },
               "start": { "character": 0, "line": 24 }
             }
           },
           "range": {
             "end": { "character": 3, "line": 28 },
             "start": { "character": 0, "line": 25 }
           }
         },
         "range": {
           "end": { "character": 4, "line": 25 },
           "start": { "character": 0, "line": 25 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 3, "line": 28 },
               "start": { "character": 0, "line": 24 }
             }
           },
           "range": {
             "end": { "character": 3, "line": 28 },
             "start": { "character": 0, "line": 25 }
           }
         },
         "range": {
           "end": { "character": 16, "line": 25 },
           "start": { "character": 5, "line": 25 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "range": {
                   "end": { "character": 3, "line": 28 },
                   "start": { "character": 0, "line": 24 }
                 }
               },
               "range": {
                 "end": { "character": 3, "line": 28 },
                 "start": { "character": 0, "line": 25 }
               }
             },
             "range": {
               "end": { "character": 3, "line": 28 },
               "start": { "character": 2, "line": 26 }
             }
           },
           "range": {
             "end": { "character": 17, "line": 26 },
             "start": { "character": 4, "line": 26 }
           }
         },
         "range": {
           "end": { "character": 17, "line": 26 },
           "start": { "character": 14, "line": 26 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 47, "line": 31 },
               "start": { "character": 0, "line": 30 }
             }
           },
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 0, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 31 },
           "start": { "character": 0, "line": 31 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 47, "line": 31 },
               "start": { "character": 0, "line": 30 }
             }
           },
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 0, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 15, "line": 31 },
           "start": { "character": 4, "line": 31 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "range": {
                 "end": { "character": 47, "line": 31 },
                 "start": { "character": 0, "line": 30 }
               }
             },
             "range": {
               "end": { "character": 47, "line": 31 },
               "start": { "character": 0, "line": 31 }
             }
           },
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 18, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 19, "line": 31 },
           "start": { "character": 18, "line": 31 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "range": {
                     "end": { "character": 47, "line": 31 },
                     "start": { "character": 0, "line": 30 }
                   }
                 },
                 "range": {
                   "end": { "character": 47, "line": 31 },
                   "start": { "character": 0, "line": 31 }
                 }
               },
               "range": {
                 "end": { "character": 47, "line": 31 },
                 "start": { "character": 18, "line": 31 }
               }
             },
             "range": {
               "end": { "character": 31, "line": 31 },
               "start": { "character": 20, "line": 31 }
             }
           },
           "range": {
             "end": { "character": 31, "line": 31 },
             "start": { "character": 30, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 32, "line": 31 },
           "start": { "character": 31, "line": 31 }
         }
       },
       {
         "range": {
           "end": { "character": 100, "line": 100 },
           "start": { "character": 100, "line": 100 }
         }
       }
     ];
     [
       {
         "parent": {
           "range": {
             "end": { "character": 7, "line": 0 },
             "start": { "character": 0, "line": 0 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 0 },
           "start": { "character": 0, "line": 0 }
         }
       }
     ]] |}]

let%expect_test "JsLIGO selection range" =
  let tests =
    [ { file_path = "contracts/lsp/selection_range.mligo"
      ; positions =
          [ (* comment *)
            { line = 0; character = 0 }
          ; { line = 0; character = 5 }
          ; (* module alias *)
            { line = 1; character = 0 }
          ; { line = 1; character = 10 }
          ; (* module *)
            { line = 4; character = 0 }
          ; { line = 6; character = 0 }
          ; (* entrypoint attribute *)
            { line = 8; character = 2 }
          ; { line = 8; character = 7 }
          ; (* type: sum *)
            { line = 12; character = 0 }
          ; { line = 12; character = 8 }
          ; (* variant *)
            { line = 13; character = 2 }
          ; { line = 13; character = 16 }
          ; (* function *)
            { line = 18; character = 0 }
          ; { line = 18; character = 10 }
          ; { line = 19; character = 12 }
          ; { line = 23; character = 16 }
          ; (* type: record *)
            { line = 30; character = 0 }
          ; { line = 30; character = 12 }
          ; { line = 30; character = 29 }
          ; (* value *)
            { line = 31; character = 0 }
          ; { line = 31; character = 11 }
          ; { line = 31; character = 18 }
          ; { line = 31; character = 31 }
          ; (* out of bounds *)
            { line = 100; character = 100 }
          ]
      }
    ]
  in
  run_multiple_tests tests ~test_runner:run_selection_range_test;
  [%expect
    {|
    [[
       {
         "parent": {
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 0 }
           }
         },
         "range": {
           "end": { "character": 8, "line": 0 },
           "start": { "character": 0, "line": 0 }
         }
       },
       {
         "parent": {
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 0 }
           }
         },
         "range": {
           "end": { "character": 8, "line": 0 },
           "start": { "character": 0, "line": 0 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 23, "line": 1 },
               "start": { "character": 0, "line": 0 }
             }
           },
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 1 }
           }
         },
         "range": {
           "end": { "character": 6, "line": 1 },
           "start": { "character": 0, "line": 1 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 23, "line": 1 },
               "start": { "character": 0, "line": 0 }
             }
           },
           "range": {
             "end": { "character": 23, "line": 1 },
             "start": { "character": 0, "line": 1 }
           }
         },
         "range": {
           "end": { "character": 11, "line": 1 },
           "start": { "character": 7, "line": 1 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 3, "line": 9 },
               "start": { "character": 0, "line": 3 }
             }
           },
           "range": {
             "end": { "character": 3, "line": 9 },
             "start": { "character": 0, "line": 4 }
           }
         },
         "range": {
           "end": { "character": 6, "line": 4 },
           "start": { "character": 0, "line": 4 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 3, "line": 9 },
               "start": { "character": 0, "line": 3 }
             }
           },
           "range": {
             "end": { "character": 3, "line": 9 },
             "start": { "character": 0, "line": 4 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 9 },
           "start": { "character": 11, "line": 4 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "range": {
                     "end": { "character": 3, "line": 9 },
                     "start": { "character": 0, "line": 3 }
                   }
                 },
                 "range": {
                   "end": { "character": 3, "line": 9 },
                   "start": { "character": 0, "line": 4 }
                 }
               },
               "range": {
                 "end": { "character": 3, "line": 9 },
                 "start": { "character": 11, "line": 4 }
               }
             },
             "range": {
               "end": { "character": 55, "line": 8 },
               "start": { "character": 2, "line": 7 }
             }
           },
           "range": {
             "end": { "character": 55, "line": 8 },
             "start": { "character": 2, "line": 8 }
           }
         },
         "range": {
           "end": { "character": 10, "line": 8 },
           "start": { "character": 2, "line": 8 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "range": {
                     "end": { "character": 3, "line": 9 },
                     "start": { "character": 0, "line": 3 }
                   }
                 },
                 "range": {
                   "end": { "character": 3, "line": 9 },
                   "start": { "character": 0, "line": 4 }
                 }
               },
               "range": {
                 "end": { "character": 3, "line": 9 },
                 "start": { "character": 11, "line": 4 }
               }
             },
             "range": {
               "end": { "character": 55, "line": 8 },
               "start": { "character": 2, "line": 7 }
             }
           },
           "range": {
             "end": { "character": 55, "line": 8 },
             "start": { "character": 2, "line": 8 }
           }
         },
         "range": {
           "end": { "character": 10, "line": 8 },
           "start": { "character": 2, "line": 8 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 15, "line": 15 },
               "start": { "character": 0, "line": 11 }
             }
           },
           "range": {
             "end": { "character": 15, "line": 15 },
             "start": { "character": 0, "line": 12 }
           }
         },
         "range": {
           "end": { "character": 4, "line": 12 },
           "start": { "character": 0, "line": 12 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 15, "line": 15 },
               "start": { "character": 0, "line": 11 }
             }
           },
           "range": {
             "end": { "character": 15, "line": 15 },
             "start": { "character": 0, "line": 12 }
           }
         },
         "range": {
           "end": { "character": 13, "line": 12 },
           "start": { "character": 5, "line": 12 }
         }
       },
       {
         "parent": {
           "range": {
             "end": { "character": 15, "line": 15 },
             "start": { "character": 0, "line": 11 }
           }
         },
         "range": {
           "end": { "character": 15, "line": 15 },
           "start": { "character": 0, "line": 12 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "range": {
                   "end": { "character": 15, "line": 15 },
                   "start": { "character": 0, "line": 11 }
                 }
               },
               "range": {
                 "end": { "character": 15, "line": 15 },
                 "start": { "character": 0, "line": 12 }
               }
             },
             "range": {
               "end": { "character": 15, "line": 15 },
               "start": { "character": 4, "line": 13 }
             }
           },
           "range": {
             "end": { "character": 21, "line": 13 },
             "start": { "character": 4, "line": 13 }
           }
         },
         "range": {
           "end": { "character": 17, "line": 13 },
           "start": { "character": 15, "line": 13 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 20, "line": 22 },
               "start": { "character": 0, "line": 17 }
             }
           },
           "range": {
             "end": { "character": 20, "line": 22 },
             "start": { "character": 0, "line": 18 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 18 },
           "start": { "character": 0, "line": 18 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 20, "line": 22 },
               "start": { "character": 0, "line": 17 }
             }
           },
           "range": {
             "end": { "character": 20, "line": 22 },
             "start": { "character": 0, "line": 18 }
           }
         },
         "range": {
           "end": { "character": 12, "line": 18 },
           "start": { "character": 4, "line": 18 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "range": {
                 "end": { "character": 20, "line": 22 },
                 "start": { "character": 0, "line": 17 }
               }
             },
             "range": {
               "end": { "character": 20, "line": 22 },
               "start": { "character": 0, "line": 18 }
             }
           },
           "range": {
             "end": { "character": 20, "line": 22 },
             "start": { "character": 2, "line": 19 }
           }
         },
         "range": {
           "end": { "character": 13, "line": 19 },
           "start": { "character": 8, "line": 19 }
         }
       },
       {
         "range": {
           "end": { "character": 16, "line": 23 },
           "start": { "character": 16, "line": 23 }
         }
       },
       {
         "parent": {
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 0, "line": 30 }
           }
         },
         "range": {
           "end": { "character": 8, "line": 30 },
           "start": { "character": 0, "line": 30 }
         }
       },
       {
         "range": {
           "end": { "character": 47, "line": 31 },
           "start": { "character": 0, "line": 30 }
         }
       },
       {
         "range": {
           "end": { "character": 47, "line": 31 },
           "start": { "character": 0, "line": 30 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 47, "line": 31 },
               "start": { "character": 0, "line": 30 }
             }
           },
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 0, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 3, "line": 31 },
           "start": { "character": 0, "line": 31 }
         }
       },
       {
         "parent": {
           "parent": {
             "range": {
               "end": { "character": 47, "line": 31 },
               "start": { "character": 0, "line": 30 }
             }
           },
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 0, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 15, "line": 31 },
           "start": { "character": 4, "line": 31 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "range": {
                 "end": { "character": 47, "line": 31 },
                 "start": { "character": 0, "line": 30 }
               }
             },
             "range": {
               "end": { "character": 47, "line": 31 },
               "start": { "character": 0, "line": 31 }
             }
           },
           "range": {
             "end": { "character": 47, "line": 31 },
             "start": { "character": 18, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 19, "line": 31 },
           "start": { "character": 18, "line": 31 }
         }
       },
       {
         "parent": {
           "parent": {
             "parent": {
               "parent": {
                 "parent": {
                   "range": {
                     "end": { "character": 47, "line": 31 },
                     "start": { "character": 0, "line": 30 }
                   }
                 },
                 "range": {
                   "end": { "character": 47, "line": 31 },
                   "start": { "character": 0, "line": 31 }
                 }
               },
               "range": {
                 "end": { "character": 47, "line": 31 },
                 "start": { "character": 18, "line": 31 }
               }
             },
             "range": {
               "end": { "character": 31, "line": 31 },
               "start": { "character": 20, "line": 31 }
             }
           },
           "range": {
             "end": { "character": 31, "line": 31 },
             "start": { "character": 30, "line": 31 }
           }
         },
         "range": {
           "end": { "character": 32, "line": 31 },
           "start": { "character": 31, "line": 31 }
         }
       },
       {
         "range": {
           "end": { "character": 100, "line": 100 },
           "start": { "character": 100, "line": 100 }
         }
       }
     ]] |}]
