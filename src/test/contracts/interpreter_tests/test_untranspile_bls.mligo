let test_fr =
  let bls = (0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f624 : bls12_381_fr) in
  let val = Test.compile_value bls in
  let dec = (Test.decompile val : bls12_381_fr) in
  assert (Test.to_string dec = Test.to_string bls)

let test_g1 =
  let bls = (0x024142bc89bf29017a38d0ee97711098639aa0bbc5b54b5104cc88b1c0fd09330fb8341e3da91e7a50f0da5c988517db0f52df51f745392ecdd3ffbb50f8a25fcdec6f48886b650de26821e244cb8ab69d49722d290a420ce1284b909d3e15a0 : bls12_381_g1) in
  let val = Test.compile_value bls in
  let dec = (Test.decompile val : bls12_381_g1) in
  assert (Test.to_string dec = Test.to_string bls)

let test_g2 =
  let bls = (0x0050b3ab4877c99ce7f180e879d91eb4df24b1e20ed88f1fdde42f91dfe0e7e451aa35d1457dd15ab507fc8f2b3180550ca7b4ea9b67810e346456c35060c8d542f37ee5fe2b1461e2f02fefac55a9863e94cab5c16befad3b866a42ee20835b1351f3f9c20a05586c1d647d756efb5c575d7ab23fbf5b3e1a6ffe024633a63a668a01fcab440866035ea2c0d4bfe30a1242f67119650e2aa605289ade2684287192382d6a01d7865fcd9e1507264a80f387b6441e37438c888159827a4efa67 : bls12_381_g2) in
  let val = Test.compile_value bls in
  let dec = (Test.decompile val : bls12_381_g2) in
  assert (Test.to_string dec = Test.to_string bls)
