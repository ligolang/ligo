function check_signature (const pk : key;
                          const signed : signature;
                          const msg: bytes) : bool
is crypto_check (pk, signed, msg)
