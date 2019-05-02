// Copyright Coase, Inc 2019

type card_pattern_id is nat
type card_pattern is record [
  coefficient : tez ;
  quantity : nat ;
  last_id : nat ;
]

type card_patterns is map(card_pattern_id , card_pattern)

type card_id is nat
type card is record [
  card_owner : address
]
type cards is map(card_id , card)

type storage_type is record [
  cards : cards ;
  card_patterns : card_patterns ;
]

type action_buy_single is record [
  card_to_buy : card_pattern_id ;
]
type action_sell_single is record [
  card_to_sell : card_id ;
]
type action_transfer is record [
  card_to_transfer : card_id ;
  destination : address ;
]

type action is
| Buy_single of action_buy_single
// | Sell of action_sell
// | Transfer of action_transfer

function buy_single(const action : action_buy_single ; const s : storage_type) : (list(operation) * storage_type) is
  begin
    const pattern : card_pattern = get_force(action.card_to_buy , s.card_patterns) ;
    const price : tez = card_pattern.coefficient * (card_pattern.quantity + 1n) ;
    if (price > amount) then fail "Not enough money" else skip ;
    const operations : list(operation) = nil ;
    pattern.quantity := pattern.quantity + 1n ;
    const card_patterns : card_patterns = s.card_patterns ;
    card_patterns[action.card_to_buy] := pattern ;
    s.card_patterns := card_patterns ;
  end with (operations , s)

function main(const action : action ; const s : storage_type) : (list(operation) * storage_type) is
  block {skip} with
  case action of
  | Buy_single abs -> buy_single (abs , s)
  // | Sell as -> sell_single (as , s)
  // | Transfer at -> transfer (at , s)
  end
