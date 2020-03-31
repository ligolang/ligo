// Dexter
// a decentralized Tezos exchange for XTZ and FA1.2
// copyright: camlCase 2019-2020
// version: 0.1.5.0

// =============================================================================
// Entrypoints
// =============================================================================

type entrypoint is
| Approve of (address * nat * nat)
| AddLiquidity of (address * nat * nat * timestamp)
| RemoveLiquidity of (address * address * nat * tez * nat * timestamp)
| XtzToToken of (address * nat * timestamp)
| TokenToXtz of (address * address * nat * tez * timestamp)
| BetForBakingRights of (key_hash * address * nat)
| EndAuctionRound
| UpdateTokenBalance of (nat)

// the transfer entrypoint of the FA1.2 contract
type token_contract_transfer is (address * address * nat);

// =============================================================================
// Storage
// =============================================================================

type baker_address is key_hash;

type account is record
  balance   : nat;
  allowances: map(address, nat);
end

// this is just to force big_maps as the first item of a pair on the top
// so we can still use the old big map route without big map id for
// convenience
type s is record
  current_baker: option(baker_address);
  current_baker_candidate: option(baker_address * address * tez * nat);
  last_auction: timestamp;
  lqt_total: nat;
  token_address: address;
  token_balance: nat;
  rewards: (tez * nat);
end

type storage is record
  s: s;
  accounts: big_map(address, account);
end

// =============================================================================
// Constants
// =============================================================================

const empty_allowances : map(address,nat) = map end;

const empty_ops : list(operation) = list end;

const no_baker_candidate: option(baker_address * address * tez * nat) = None;

const no_baker: option(key_hash) = None;

// 21 days
// 86400 seconds * 21 
const dexter_cycle: int = 1814400;

// =============================================================================
// Helper Functions
// =============================================================================

function mutez_to_natural(const a: tez): nat is
  block {skip} with a / 1mutez

function natural_to_mutez(const a: nat): tez is
  block {skip} with a * 1mutez

// this will fail if provided a negative number
function int_to_nat(const error: string ; const a: int): nat is
  block {
    var result : nat := 0n;
    if (a >= 0) then block {
      result := abs(a);
    } else block {
      failwith(error)
    };    
  } with result;

// get an account from the big_map, if one does not exist for a particular
// address then create one.
function get_account(const a: address ; const m: big_map(address, account)): account is
  block { skip } with (
    case (m[a]) of
      | None          -> record balance = 0n; allowances = empty_allowances; end
      | Some(account) -> account
    end
  );

function update_allowance(const owner            : address;
                          const spender          : address;
                          const updated_allowance: nat;
                          var   storage          : storage):
                          storage is
  block {
    if (spender =/= owner) then block {
      var account: account        := get_account(owner, storage.accounts);
      account.allowances[spender] := updated_allowance;
      storage.accounts[owner]       := record balance = account.balance; allowances = account.allowances; end;
    } else {
      skip;
    }
  } with storage;

// if sender is owner, return amount, otherwise check if sender has permission
// if true then return amount, otherwise fail
function get_sender_allowance(const owner: address ; const storage: storage): nat is
  block {
    var result: nat := 0n;
    case storage.accounts[owner] of
    | None -> failwith("2")
    | Some(account) -> block {
        if sender =/= owner then block {
          case account.allowances[sender] of
            | None -> failwith("3")
            | Some(allowance) -> result := allowance
          end;
        } else block {
          result := account.balance
        }
      }
    end;
  } with result;

function get_current_candidate_bet (const storage: storage): (tez * nat) is
  block {
    var current_candidate_bet: (tez * nat) := (0mutez, 0n);
    case (storage.s.current_baker_candidate) of
      | None   -> skip
      | Some(current_baker_candidate) -> current_candidate_bet := (current_baker_candidate.2, current_baker_candidate.3)
    end
  } with current_candidate_bet;

// there might be some zero division edge cases
function get_xtz_pool (const current_candidate_bet_xtz: tez ; const storage: storage): (tez) is
  block {
    const time_since_last_auction: int = now - storage.s.last_auction;
    const days_since_last_auction: int = time_since_last_auction / 86400;
    var xtz_pool : tez := 0mutez;
    
    if (days_since_last_auction < dexter_cycle) then block {
      const released_rewards  : tez = (storage.s.rewards.0 / abs((days_since_last_auction * 1000 / dexter_cycle))) / 1000n;
      const unreleased_rewards: tez = storage.s.rewards.0 - released_rewards;

      xtz_pool := balance - current_candidate_bet_xtz - unreleased_rewards - amount;
    } else block {
      // the slow reward wait has passed, all the rewards are released    

      xtz_pool := balance - current_candidate_bet_xtz - amount;
    };
  } with xtz_pool;

// there might be some zero division edge cases
function get_token_pool (const current_candidate_bet_token: nat ; const storage: storage): (nat) is
  block {
    const time_since_last_auction: int = now - storage.s.last_auction;
    const days_since_last_auction: int = time_since_last_auction / 86400;
    var token_pool : nat := 0n;
    
    if (days_since_last_auction < dexter_cycle) then block {
      const reward_days : int = days_since_last_auction;
      const released_rewards  : nat = (storage.s.rewards.1 / abs((reward_days * 1000 / dexter_cycle))) / 1000n;
      const unreleased_rewards: nat = abs(storage.s.rewards.1 - released_rewards);

      token_pool := abs(storage.s.token_balance - current_candidate_bet_token - unreleased_rewards);
    } else block {
      // the slow reward wait has passed, all the rewards are released
      token_pool := abs(storage.s.token_balance - current_candidate_bet_token)
    };
  } with token_pool;

// =============================================================================
// Entrypoint Functions
// =============================================================================

function approve(const spender  : address;
                 const allowance: nat;
                 const current_allowance: nat;
                 var storage      : storage):
                 (list(operation) * storage) is
  block {
    if (spender =/= sender) then block {
      // get the sender's account
      // if the account does not exist, fail, we do not want to create accounts here
      // creating accounts should be done in add_liquidity
      const account: account = get_account(sender, storage.accounts);

      var sender_allowances: map(address, nat) := account.allowances;
      sender_allowances[spender] := allowance;
      storage.accounts[sender]  := record balance = account.balance; allowances = sender_allowances; end;
    } else block {
      failwith("1");
    }
  } with (empty_ops, storage);

// it is assumed that the exchange contract has permission from the FA1.2 token
// to manage the assets of the user. It is the responsibility of the dApp
// developer to handle permissions.
function add_liquidity(const owner               : address;
                       const min_lqt_created     : nat;
                       const max_tokens_deposited: nat;
                       const deadline            : timestamp;
                       var   storage               : storage):
                       (list(operation) * storage) is
  block {
    // add_liquidity performs a transfer to the token contract, we need to
    // return the operations
    var op_list: list(operation) := nil;
    
    if (now < deadline) then skip else block {
      failwith("4");
    };

    if (max_tokens_deposited > 0n) then skip else block {
      failwith("5");
    };   

    if (amount > 0mutez) then skip else block {
      failwith("6");
    };
    
    if (storage.s.lqt_total > 0n) then block {
      // lqt_total greater than zero
      // use the existing exchange rate

      if (min_lqt_created > 0n) then skip else block {
        failwith("7");
      };

      const current_candidate_bet: (tez * nat) = get_current_candidate_bet(storage);
      const xtz_pool             : nat = mutez_to_natural(get_xtz_pool(current_candidate_bet.0, storage));
      const token_pool           : nat = get_token_pool(current_candidate_bet.1, storage);
      const nat_amount           : nat = mutez_to_natural(amount);
      const tokens_deposited     : nat = nat_amount * token_pool          / xtz_pool;
      const lqt_minted           : nat = nat_amount * storage.s.lqt_total / xtz_pool;

      if (max_tokens_deposited >= tokens_deposited) then skip else block {
        failwith("8");
      };
      
      if (lqt_minted >= min_lqt_created) then skip else block {
        failwith("9");
      };
      
      const account: account   = get_account(owner, storage.accounts);
      const new_balance: nat   = account.balance + lqt_minted;
      storage.accounts[owner] := record balance = new_balance; allowances = account.allowances; end;
      storage.s.lqt_total     := storage.s.lqt_total + lqt_minted;
      storage.s.token_balance := storage.s.token_balance + tokens_deposited;

      // send FA1.2 from owner to exchange
      const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
      const op1: operation = transaction((owner, self_address, tokens_deposited), 0mutez, token_contract);
      op_list := list op1; end;

    } else block {
      // initial add liquidity
      if (amount >= 1tz) then skip  else block {
        failwith("10");
      };

      const tokens_deposited     : nat = max_tokens_deposited;
      const current_candidate_bet: (tez * nat) = get_current_candidate_bet(storage);
      const initial_liquidity    : nat = mutez_to_natural(balance - current_candidate_bet.0);

      storage.s.lqt_total     := initial_liquidity;
      storage.accounts[owner] := record balance = initial_liquidity; allowances = empty_allowances; end;
      storage.s.token_balance := tokens_deposited;

      // send FA1.2 tokens from owner to exchange
      const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
      const op1: operation = transaction((owner, self_address, tokens_deposited), 0mutez, token_contract);
      op_list := list op1; end;
    }
  } with (op_list, storage);

function remove_liquidity(const owner                : address;
                          const to_                  : address;
                          const lqt_burned           : nat;
                          const min_xtz_withdrawn    : tez;
                          const min_tokens_withdrawn : nat;
                          const deadline             : timestamp;
                          var   storage                : storage):
                          (list(operation) * storage) is
  block {
    var op_list: list(operation) := nil;
    if (now < deadline) then skip else block {
      failwith("11");
    };
    
    if (min_xtz_withdrawn > 0mutez) then skip else block {
      failwith("12");
    };

    if (min_tokens_withdrawn > 0n) then skip else block {
      failwith("13");
    };

    if (lqt_burned > 0n) then skip else block {
      failwith("14");
    };

    // returns total if sender is owner, otherwise looks it up
    const lqt: nat = get_sender_allowance(owner, storage);

    if (lqt >= lqt_burned) then skip else block {
      failwith("15");              
    };

    if (storage.s.lqt_total > 0n) then skip else block {
      failwith("16");
    };

    const current_candidate_bet: (tez * nat) = get_current_candidate_bet(storage);
    const xtz_withdrawn        : tez = natural_to_mutez(lqt_burned * mutez_to_natural(balance - current_candidate_bet.0) / storage.s.lqt_total);

    if (xtz_withdrawn >= min_xtz_withdrawn) then skip else block {
      failwith("17");
    };              

    const token_pool      : nat = get_token_pool(current_candidate_bet.1, storage);
    const tokens_withdrawn: nat = lqt_burned * token_pool / storage.s.lqt_total;

    if (tokens_withdrawn >= min_tokens_withdrawn) then skip else block {
      failwith("18");
    };

    const account: account = get_account(owner, storage.accounts);

    if (account.balance >= lqt_burned) then skip else block {
      failwith("19");
    };

    const new_balance: nat = int_to_nat("33", account.balance - lqt_burned);
    storage.accounts[owner] := record balance = new_balance; allowances = account.allowances; end;
                                    
    storage.s.lqt_total     := int_to_nat("34", storage.s.lqt_total - lqt_burned);
    storage.s.token_balance := int_to_nat("35", storage.s.token_balance - tokens_withdrawn);

    // update allowance
    // lqt - lqt_burned is safe, we have already checed that lqt >= lqt_burned
    storage := update_allowance(owner, sender, int_to_nat("36", lqt - lqt_burned), storage);
                    
    // send xtz_withdrawn to to_ address
    const to_contract: contract(unit) = get_contract(to_);
    const op1: operation = transaction(unit, xtz_withdrawn, to_contract);

    // send tokens_withdrawn to to address
    // if tokens_withdrawn if greater than storage.s.token_balance, this will fail
    const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
    const op2: operation = transaction((self_address, to_, tokens_withdrawn), 0mutez, token_contract);
    op_list := list op1; op2; end
  } with (op_list, storage);

function xtz_to_token(const to_              : address;
                      const min_tokens_bought: nat;
                      const deadline         : timestamp;
                      var storage              : storage):                      
                      (list(operation) * storage) is
  block {
    var op_list: list(operation) := nil;
    if (now < deadline) then skip else block {
      failwith("20");
    };

    const current_candidate_bet: (tez * nat) = get_current_candidate_bet(storage);
    const xtz_pool             : nat = mutez_to_natural(get_xtz_pool(current_candidate_bet.0, storage));
    const nat_amount           : nat = mutez_to_natural(amount);
    const token_pool           : nat = get_token_pool(current_candidate_bet.1, storage);
    const tokens_bought        : nat = (nat_amount * 997n * token_pool) / (xtz_pool * 1000n + (nat_amount * 997n));
      
    if (tokens_bought >= min_tokens_bought) then skip else block {
      failwith("21");
    };

    storage.s.token_balance := int_to_nat("32", storage.s.token_balance - tokens_bought);

    // send tokens_withdrawn to to address
    // if tokens_bought is greater than storage.s.token_balance, this will fail
    const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
    const op: operation = transaction((self_address, to_, tokens_bought), 0mutez, token_contract);

    // append internal operations
    op_list := list op; end;
  } with (op_list, storage);

function token_to_xtz(const owner         : address; // the address of the owner of FA1.2
                      const to_           : address;
                      const tokens_sold   : nat;
                      const min_xtz_bought: tez;
                      const deadline      : timestamp;
                      var storage           : storage):                      
                      (list(operation) * storage) is
  block {
    var op_list: list(operation) := nil;
    if (now < deadline) then skip else block {
      failwith("22");
    };

    const current_candidate_bet: (tez * nat) = get_current_candidate_bet(storage);
    const xtz_pool             : tez = get_xtz_pool(current_candidate_bet.0, storage);
    const token_pool           : nat = get_token_pool(current_candidate_bet.1, storage);    
    const xtz_bought           : tez = natural_to_mutez((tokens_sold * 997n * mutez_to_natural(xtz_pool)) / (token_pool * 1000n + (tokens_sold * 997n)));

    if (xtz_bought >= min_xtz_bought) then skip else block {
      failwith("23");
    };

    storage.s.token_balance      := storage.s.token_balance + tokens_sold;

    // send xtz_bought to to_ address
    const to_contract: contract(unit) = get_contract(to_);
    const op1: operation = transaction(unit, xtz_bought, to_contract);

    // send tokens_sold to the exchange address
    // this assumes that the exchange has an allowance for the token and owner in FA1.2
    const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
    const op2: operation = transaction((owner, self_address, tokens_sold), 0mutez, token_contract);

    // append internal operations
    op_list := list op1; op2; end;
  } with (op_list, storage);

function assert_valid_baker (const current_baker: option(key_hash);
                             const candidate: key_hash): (operation * operation) is
  block {
    // test the candidate baker, if it is valid this will not fail
    const test_set_delegate_operation: operation = set_delegate(Some(candidate));

    // reset to the current baker
    const reset_set_delegate_operation: operation = set_delegate(current_baker);
  } with (test_set_delegate_operation, reset_set_delegate_operation);

function bet_for_baking_rights (const candidate      : key_hash;
                                const token_source   : address;
                                const max_tokens_bet : nat;
                                var storage            : storage):
                                (list(operation) * storage) is
  block {
    var op_list: list(operation) := nil;

    // this is a trick to assert that the provided baker address is valid
    case (storage.s.current_baker_candidate) of
    | None -> block {
        const op_pair: (operation * operation) = assert_valid_baker(storage.s.current_baker, candidate);
        op_list := op_pair.0 # op_list;
        op_list := op_pair.1 # op_list;
      }
    | Some(current_baker_candidate) -> block {
        if (current_baker_candidate.0 = candidate) then skip else block {
          const op_pair: (operation * operation) = assert_valid_baker(storage.s.current_baker, candidate);
          op_list := op_pair.0 # op_list;
          op_list := op_pair.1 # op_list;
        };
      }
    end;

    // now we are sure it is a valid baker

    // set a minimum bet
    if (max_tokens_bet > 0n) then skip else block {
      failwith("24");
    };

    if (amount > 0mutez) then skip else block {
      failwith("25");
    };

    const current_candidate_bet : (tez * nat) = get_current_candidate_bet(storage);

    if (amount > current_candidate_bet.0) then skip else { failwith("26") };

    const xtz_pool         : nat = mutez_to_natural(get_xtz_pool(current_candidate_bet.0, storage));
    const token_pool       : nat = get_token_pool(current_candidate_bet.1, storage);
    const nat_amount       : nat = mutez_to_natural(amount);
    const tokens_deposited : nat = nat_amount * token_pool / xtz_pool;

    if (tokens_deposited > current_candidate_bet.1) then skip else { failwith("27") };
    if (tokens_deposited > max_tokens_bet) then skip else { failwith("28") };

    case (storage.s.current_baker_candidate) of
      | None -> block {
        // add the tokens_deposited to the token_balance
        storage.s.token_balance := storage.s.token_balance + tokens_deposited;
      }
      | Some(current_baker_candidate) -> block {
        // return rejected candidates tez and tokens to previous candidate
        const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
        const return_token_op: operation = transaction((self_address, current_baker_candidate.1, current_baker_candidate.3), 0mutez, token_contract);
        op_list := return_token_op # op_list;

        const to_contract: contract(unit) = get_contract(current_baker_candidate.1);
        const return_xtz_op: operation = transaction(unit, current_baker_candidate.2, to_contract);
        op_list := return_xtz_op # op_list;

        // remove the tokens from the current_baker_candidate
        // add the tokens_deposited to the token_balance
        storage.s.token_balance := abs(storage.s.token_balance - current_baker_candidate.3) + tokens_deposited;
      }
    end;

    storage.s.current_baker_candidate := Some((candidate,token_source,amount,tokens_deposited));

    // send FA1.2 from owner to exchange, dexter needs permission to transfer these tokens
    const token_contract: contract(token_contract_transfer) = get_entrypoint("%transfer", storage.s.token_address);
    const xtz_to_dexter_op: operation = transaction((token_source, self_address, tokens_deposited), 0mutez, token_contract);
    op_list := xtz_to_dexter_op # op_list;
    
  } with (op_list, storage);

function end_auction_round(var storage : storage) : (list(operation) * storage) is
  block {
    var op_list: list(operation) := nil;
    // 604800 seconds is one week
    if (now > storage.s.last_auction + 604800) then skip else {failwith("29")};

    case (storage.s.current_baker_candidate) of
      | None -> block {
          const set_delegate_op: operation = set_delegate(no_baker);
          op_list := set_delegate_op # op_list;
        }
      | Some(current_baker_candidate) -> block {
          case (storage.s.current_baker) of
            | None -> block {
                storage.s.current_baker := Some(current_baker_candidate.0);
                storage.s.current_baker_candidate := no_baker_candidate;
                storage.s.token_balance := storage.s.token_balance + current_baker_candidate.3;
                storage.s.last_auction  := now;
                const set_delegate_op: operation = set_delegate(storage.s.current_baker);
                op_list := set_delegate_op # op_list;
              }
            | Some(current_baker) -> block {
                if (current_baker = current_baker_candidate.0) then block {
                  storage.s.current_baker_candidate := no_baker_candidate;
                  storage.s.token_balance := storage.s.token_balance + current_baker_candidate.3;
                  storage.s.last_auction  := now;
                } else {
                  storage.s.current_baker := Some(current_baker_candidate.0);
                  storage.s.current_baker_candidate := no_baker_candidate;
                  storage.s.token_balance := storage.s.token_balance + current_baker_candidate.3;
                  storage.s.last_auction  := now;
                  const set_delegate_op: operation = set_delegate(storage.s.current_baker);
                  op_list := set_delegate_op # op_list;
                }
              }
          end;
        }
    end;
    
  } with (op_list, storage);

function update_token_balance(const token_balance: nat ; var storage : storage) : (list(operation) * storage) is
  block {
    var op_list: list(operation) := nil;
    if (sender =/= storage.s.token_address) then {
      failwith("31");
    }  else {
      storage.s.token_balance := token_balance;
    }
  } with (op_list, storage);

// =============================================================================
// Main
// =============================================================================

function main (const entrypoint : entrypoint ; const storage : storage) : (list(operation) * storage) is
  (case entrypoint of
  | Approve(xs)            -> approve(xs.0,xs.1,xs.2,storage)
  | AddLiquidity(xs)       -> add_liquidity(xs.0,xs.1,xs.2,xs.3,storage)
  | RemoveLiquidity(xs)    -> remove_liquidity(xs.0,xs.1,xs.2,xs.3,xs.4,xs.5,storage)
  | XtzToToken(xs)         -> xtz_to_token(xs.0,xs.1,xs.2,storage)
  | TokenToXtz(xs)         -> token_to_xtz(xs.0,xs.1,xs.2,xs.3,xs.4,storage)
  | BetForBakingRights(xs) -> bet_for_baking_rights(xs.0,xs.1,xs.2,storage)
  | EndAuctionRound        -> end_auction_round(storage)
  | UpdateTokenBalance(xs) -> update_token_balance(xs, storage)
  end);
