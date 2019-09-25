#!/bin/sh
set -e
compiled_contract=$(./scripts/ligo_ci.sh compile-contract src/test/contracts/website2.ligo main);
compiled_storage=$(./scripts/ligo_ci.sh compile-storage src/test/contracts/website2.ligo main 1);
compiled_parameter=$(./scripts/ligo_ci.sh compile-parameter src/test/contracts/website2.ligo main "Increment(1)");
dry_run_output=$(./scripts/ligo_ci.sh dry-run src/test/contracts/website2.ligo main "Increment(1)" 1);

expected_compiled_contract="{ parameter (or int int) ;
  storage int ;
  code { {} ;
         {} ;
         {} ;
         { PUSH (lambda (pair int int) int)
                { {} ;
                  {} ;
                  {} ;
                  { { { DUP ; DIP { {} } } ; CAR } ;
                    { { { { DIP { DUP } ; SWAP } ; DIP { {} } } ; CDR } ;
                      { PUSH unit Unit ;
                        DROP ;
                        { { { DIP { DUP } ; SWAP } ; DIP { { DUP ; DIP { {} } } } } ;
                          ADD } } ;
                      {} ;
                      DIP { DROP } } ;
                    {} ;
                    DIP { DROP } } ;
                  {} ;
                  DIP { DROP } ;
                  {} } ;
           { PUSH (lambda (pair int int) int)
                  { {} ;
                    {} ;
                    {} ;
                    { { { DUP ; DIP { {} } } ; CAR } ;
                      { { { { DIP { DUP } ; SWAP } ; DIP { {} } } ; CDR } ;
                        { PUSH unit Unit ;
                          DROP ;
                          { { { DIP { DUP } ; SWAP } ; DIP { { DUP ; DIP { {} } } } } ;
                            SUB } } ;
                        {} ;
                        DIP { DROP } } ;
                      {} ;
                      DIP { DROP } } ;
                    {} ;
                    DIP { DROP } ;
                    {} } ;
             { { { { DIP { { DIP { DUP } ; SWAP } } ; SWAP } ; DIP { {} } } ;
                 CAR } ;
               { { { { DIP { { DIP { { DIP { DUP } ; SWAP } } ; SWAP } } ; SWAP } ;
                     DIP { {} } } ;
                   CDR } ;
                 { PUSH unit Unit ;
                   DROP ;
                   { { NIL operation ;
                       DIP { { { { DIP { DUP } ; SWAP } ;
                                 IF_LEFT
                                   { { { DUP ;
                                         { { { { DIP { { DIP { DUP } ; SWAP } } ; SWAP } ;
                                               DIP { { DUP ; DIP { {} } } } } ;
                                             PAIR } ;
                                           DIP { { DIP { { DIP { { DIP { { DIP { DUP } ; SWAP } } ; SWAP } } ; SWAP } } ;
                                                   SWAP } } ;
                                           EXEC } ;
                                         {} ;
                                         DIP { DROP } } ;
                                       {} ;
                                       DIP { DROP } } }
                                   { { { DUP ;
                                         { { { { DIP { { DIP { DUP } ; SWAP } } ; SWAP } ;
                                               DIP { { DUP ; DIP { {} } } } } ;
                                             PAIR } ;
                                           DIP { { DIP { { DIP { { DIP { { DIP { { DIP { DUP } ; SWAP } } ; SWAP } } ; SWAP } } ;
                                                           SWAP } } ;
                                                   SWAP } } ;
                                           EXEC } ;
                                         {} ;
                                         DIP { DROP } } ;
                                       {} ;
                                       DIP { DROP } } } } ;
                               DIP { {} } } } } ;
                     PAIR } } ;
                 {} ;
                 DIP { DROP } } ;
               {} ;
               DIP { DROP } } ;
             {} ;
             DIP { DROP } } ;
           {} ;
           DIP { DROP } } ;
         {} ;
         DIP { DROP } ;
         {} } }";
expected_compiled_parameter="(Right 1)";
expected_compiled_storage=1;
expected_dry_run_output="tuple[   list[]
         2
]";

if [ "$compiled_contract" != "$expected_compiled_contract" ]; then
    echo "Expected $expected_compiled_contract as compile-storage output, got $compiled_contract instead";
    exit 1;
fi

if [ "$compiled_storage" != "$expected_compiled_storage" ]; then
    echo "Expected $expected_compiled_storage as compile-storage output, got $compiled_storage instead";
    exit 1;
fi

if [ "$compiled_parameter" != "$expected_compiled_parameter" ]; then
    echo "Expected $expected_compiled_parameter as compile-parameter output, got $compiled_parameter instead";
    exit 1;
fi

if [ "$dry_run_output" != "$expected_dry_run_output" ]; then
    echo "Expected $expected_dry_run_output as dry-run output, got $dry_run_output instead";
    exit 1;
fi

echo "CLI tests passed";