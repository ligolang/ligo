#import "../../c/d/foo.jsligo" "Foo"

const _test_originate_from_file_relative_path = () : typed_address<unit, unit> => {
  let x = Test.originate_from_file ("../../../src/contract/unit.mligo", unit, 0 as mutez);
  return x.addr
};

const test_originate_from_file_relative_path = _test_originate_from_file_relative_path();


const _test_originate_from_file_relative_path_w_r_t_imported_file = () : bool => {
  let addr = Foo.originate();
  let bef  = Test.get_balance(addr);
  ignore(Test.transfer (addr, unit, 10 as mutez));
  let aft  = Test.get_balance(addr);
  return aft == (bef + (10 as mutez))
}

const test_originate_from_file_relative_path_w_r_t_imported_file =
    _test_originate_from_file_relative_path_w_r_t_imported_file();