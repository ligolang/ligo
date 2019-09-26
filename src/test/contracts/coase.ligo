// Copyright Coase, Inc 2019

type card_pattern_id is nat
type card_pattern is record [
  coefficient : tez ;
  quantity : nat ;
]

type card_patterns is map(card_pattern_id , card_pattern)

type card_id is nat
type card is record [
  card_owner : address ;
  card_pattern : card_pattern_id ;
]
type cards is map(card_id , card)

type storage_type is record [
  cards : cards ;
  card_patterns : card_patterns ;
  next_id : nat ;
]

type action_buy_single is record [
  card_to_buy : card_pattern_id ;
]
type action_sell_single is record [
  card_to_sell : card_id ;
]
type action_transfer_single is record [
  card_to_transfer : card_id ;
  destination : address ;
]

type action is
| Buy_single of action_buy_single
| Sell_single of action_sell_single
| Transfer_single of action_transfer_single

function transfer_single(const action : action_transfer_single ; const s : storage_type) : (list(operation) * storage_type) is
  begin
    const cards : cards = s.cards ;
    const card : card = get_force(action.card_to_transfer , cards) ;
    if (card.card_owner =/= source) then fail "This card doesn't belong to you" else skip ;
    card.card_owner := action.destination ;
    cards[action.card_to_transfer] := card ;
    s.cards := cards ;
    const operations : list(operation) = nil ;
  end with (operations , s) ;

function sell_single(const action : action_sell_single ; const s : storage_type) : (list(operation) * storage_type) is
  begin
    const card : card = get_force(action.card_to_sell , s.cards) ;
    if (card.card_owner =/= source) then fail "This card doesn't belong to you" else skip ;
    const card_pattern : card_pattern = get_force(card.card_pattern , s.card_patterns) ;
    card_pattern.quantity := abs(card_pattern.quantity - 1n);
    const card_patterns : card_patterns = s.card_patterns ;
    card_patterns[card.card_pattern] := card_pattern ;
    s.card_patterns := card_patterns ;
    const cards : cards = s.cards ;
    remove action.card_to_sell from map cards ;
    s.cards := cards ;
    const price : tez = card_pattern.coefficient * card_pattern.quantity ;
    const receiver : contract(unit) = get_contract(source) ;
    const op : operation = transaction(unit , price , receiver) ;
    const operations : list(operation) = list op end ;
  end with (operations , s)

function buy_single(const action : action_buy_single ; const s : storage_type) : (list(operation) * storage_type) is
  begin
    // Check funds
    const card_pattern : card_pattern = get_force(action.card_to_buy , s.card_patterns) ;
    const price : tez = card_pattern.coefficient * (card_pattern.quantity + 1n) ;
    if (price > amount) then fail "Not enough money" else skip ;
    // Administrative procedure
    const operations : list(operation) = nil ;
    // Increase quantity
    card_pattern.quantity := card_pattern.quantity + 1n ;
    const card_patterns : card_patterns = s.card_patterns ;
    card_patterns[action.card_to_buy] := card_pattern ;
    s.card_patterns := card_patterns ;
    // Add card
    const cards : cards = s.cards ;
    cards[s.next_id] := record
      card_owner = source ;
      card_pattern = action.card_to_buy ;
    end ;
    s.cards := cards ;
    s.next_id := s.next_id + 1n ;
  end with (operations , s)

function main(const action : action ; const s : storage_type) : (list(operation) * storage_type) is
  block {skip} with
  case action of
  | Buy_single (bs) -> buy_single (bs , s)
  | Sell_single (as) -> sell_single (as , s)
  | Transfer_single (at) -> transfer_single (at , s)
  end
