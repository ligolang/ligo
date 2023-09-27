#import "tezos-ligo-fa2/lib/fa2/asset/single_asset.mligo" "SingleAsset"
#import "tezos-ligo-fa2/test/fa2/single_asset.test.mligo" "SingleAsset_helper"

let test =
    let f = "tezos-ligo-fa2/lib/fa2/asset/single_asset.mligo" in
    let init_storage, owners, ops = SingleAsset_helper.get_initial_storage(10n, 10n, 10n) in
    let orig : (SingleAsset parameter_of, SingleAsset.storage) origination_result = Test.originate_from_file f init_storage 0tez in
    Test.to_contract orig.addr
