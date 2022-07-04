#define POUET

#if POUET
let chr (n : nat) : string  =
     let backslash = [%external ("TEST_UNESCAPE_STRING", "\\")] in
    backslash
#endif
