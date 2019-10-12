const add_tez : tez = 21mtz + 0.000021tz;
const sub_tez : tez = 21mtz - 20mtz;
(* This is not enough. *)
const not_enough_tez : tez = 4611686018427387903mtz;


const nat_mul_tez : tez = 1n * 100mtz;
const tez_mul_nat : tez = 100mtz * 10n;

const tez_div_tez1 : nat = 100mtz / 1mtz;
const tez_div_tez2 : nat = 100mtz / 90mtz;
const tez_div_tez3 : nat = 100mtz / 110mtz;

const tez_mod_tez1 : tez = 100mtz mod 1mtz;
const tez_mod_tez2 : tez = 100mtz mod 90mtz;
const tez_mod_tez3 : tez = 100mtz mod 110mtz;
