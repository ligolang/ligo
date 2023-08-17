#import "tezos-ligo-fa2/lib/fa2/asset/single_asset.mligo" "SingleAsset"
#import "tezos-ligo-fa2/test/fa2/single_asset.test.mligo" "SingleAsset_helper"

let test =
    let f = "tezos-ligo-fa2/lib/fa2/asset/single_asset.mligo" in
    let init_storage, owners, ops = SingleAsset_helper.get_initial_storage(10n, 10n, 10n) in
    let v_mich = Test.run (fun (x:SingleAsset.Storage.t) -> x) init_storage in
    let (addr, _, _) = Test.originate_from_file f "main" v_mich 0tez in
    let taddr : (SingleAsset.parameter, SingleAsset.storage) typed_address = Test.cast_address addr in
    let contr = Test.to_contract taddr in
    contr
