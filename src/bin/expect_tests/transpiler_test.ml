open Cli_expect

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/coase.ligo" ; "pascaligo" ] ;
  [%expect {|
    type card_pattern_id is nat

    type card_pattern is
      record [quantity : nat; coefficient : tez]

    type card_patterns is map (card_pattern_id, card_pattern)

    type card_id is nat

    type card is
      record [
        card_pattern : card_pattern_id;
        card_owner : address
      ]

    type cards is map (card_id, card)

    type storage is
      record [
        next_id : nat;
        cards : cards;
        card_patterns : card_patterns
      ]

    type return is list (operation) * storage

    type action_buy_single is
      record [card_to_buy : card_pattern_id]

    type action_sell_single is record [card_to_sell : card_id]

    type action_transfer_single is
      record [destination : address; card_to_transfer : card_id]

    type parameter is
        Transfer_single of action_transfer_single
      | Sell_single of action_sell_single
      | Buy_single of action_buy_single

    function transfer_single
      (const gen__parameters4 : action_transfer_single * storage) is
      case gen__parameters4 of [
        (action, s) ->
          (block {
             const cards : cards = s.cards;
             const card : card
             = case cards [action.card_to_transfer] of [
                 Some (card) -> card
               | None ->
                   (failwith ("transfer_single: No card.")
                    : card)
               ];
             if NEQ (card.card_owner, Tezos.sender)
             then failwith ("This card doesn't belong to you")
             else skip;
             card.card_owner := action.destination;
             cards [action.card_to_transfer] := card;
             s.cards := cards
           } with ((list [] : list (operation)), s)
           : return)
      ]

    function sell_single
      (const gen__parameters3 : action_sell_single * storage) is
      case gen__parameters3 of [
        (action, s) ->
          (block {
             const card : card
             = case s.cards [action.card_to_sell] of [
                 Some (card) -> card
               | None ->
                   (failwith ("sell_single: No card.") : card)
               ];
             if NEQ (card.card_owner, Tezos.sender)
             then failwith ("This card doesn't belong to you")
             else skip;
             const card_pattern : card_pattern
             = case s.card_patterns [card.card_pattern] of [
                 Some (pattern) -> pattern
               | None ->
                   (failwith ("sell_single: No card pattern.")
                    : card_pattern)
               ];
             card_pattern.quantity :=
               abs (SUB (card_pattern.quantity, 1n));
             const card_patterns : card_patterns
             = s.card_patterns;
             card_patterns [card.card_pattern] := card_pattern;
             s.card_patterns := card_patterns;
             const cards : cards = s.cards;
             const cards : _
             = Map.remove (action.card_to_sell, cards);
             s.cards := cards;
             const price : tez
             = TIMES
                 (card_pattern.coefficient,
                  card_pattern.quantity);
             const receiver : contract (unit)
             = case (Tezos.get_contract_opt (Tezos.sender)
                     : option (contract (unit)))
               of [
                 Some (contract) -> contract
               | None ->
                   (failwith ("sell_single: No contract.")
                    : contract (unit))
               ];
             const op : operation
             = Tezos.transaction (unit, price, receiver);
             const operations : list (operation) = list [op]
           } with (operations, s)
           : return)
      ]

    function buy_single
      (const gen__parameters2 : action_buy_single * storage) is
      case gen__parameters2 of [
        (action, s) ->
          (block {
             const card_pattern : card_pattern
             = case s.card_patterns [action.card_to_buy] of [
                 Some (pattern) -> pattern
               | None ->
                   (failwith ("buy_single: No card pattern.")
                    : card_pattern)
               ];
             const price : tez
             = TIMES
                 (card_pattern.coefficient,
                  ADD (card_pattern.quantity, 1n));
             if GT (price, Tezos.amount)
             then failwith ("Not enough money")
             else skip;
             card_pattern.quantity :=
               ADD (card_pattern.quantity, 1n);
             const card_patterns : card_patterns
             = s.card_patterns;
             card_patterns [action.card_to_buy] := card_pattern;
             s.card_patterns := card_patterns;
             const cards : cards = s.cards;
             cards [s.next_id] :=
               record [
                 card_pattern = action.card_to_buy;
                 card_owner = Tezos.sender
               ];
             s.cards := cards;
             s.next_id := ADD (s.next_id, 1n)
           } with ((list [] : list (operation)), s)
           : return)
      ]

    function main (const gen__parameters1 : parameter * storage) is
      case gen__parameters1 of [
        (action, s) ->
          (case action of [
             Buy_single (bs) -> buy_single (bs, s)
           | Sell_single (as) -> sell_single (as, s)
           | Transfer_single (at) -> transfer_single (at, s)
           ]
           : return)
      ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/coase.ligo" ; "cameligo" ] ;
  [%expect {|
    type card_pattern_id = nat

    type card_pattern = {quantity : nat; coefficient : tez}

    type card_patterns = (card_pattern_id, card_pattern) map

    type card_id = nat

    type card =
      {card_pattern : card_pattern_id; card_owner : address}

    type cards = (card_id, card) map

    type storage =
      {next_id : nat;
       cards : cards;
       card_patterns : card_patterns}

    type return = operation list * storage

    type action_buy_single = {card_to_buy : card_pattern_id}

    type action_sell_single = {card_to_sell : card_id}

    type action_transfer_single =
      {destination : address; card_to_transfer : card_id}

    type parameter =
      Transfer_single of action_transfer_single
    | Sell_single of action_sell_single
    | Buy_single of action_buy_single

    let transfer_single
    : action_transfer_single * storage -> return =
      (fun gen__parameters4 : action_transfer_single * storage ->
         match gen__parameters4 with
         action : action_transfer_single, s : storage ->
             (let cards : cards = s.cards in
              let card : card =
                match Map.find_opt action.card_to_transfer cards
                with
                  Some card -> card
                | None ->
                    ((failwith ("transfer_single: No card."))
                     : card) in
              begin
                if (NEQ (card.card_owner) (Tezos.sender))
                then
                  (failwith ("This card doesn't belong to you"))
                else ();
                let card : _ =
                  {card with
                    {card_owner = action.destination}} in
                let cards : _ =
                  Map.add card action.card_to_transfer cards in
                let s : _ = {s with {cards = cards}} in
                ([] : operation list), s
              end
              : return))

    let sell_single : action_sell_single * storage -> return =
      (fun gen__parameters3 : action_sell_single * storage ->
         match gen__parameters3 with
         action : action_sell_single, s : storage ->
             (let card : card =
                match Map.find_opt action.card_to_sell s.cards
                with
                  Some card -> card
                | None ->
                    ((failwith ("sell_single: No card."))
                     : card) in
              begin
                if (NEQ (card.card_owner) (Tezos.sender))
                then
                  (failwith ("This card doesn't belong to you"))
                else ();
                let card_pattern : card_pattern =
                  match Map.find_opt
                          card.card_pattern
                          s.card_patterns
                  with
                    Some pattern -> pattern
                  | None ->
                      ((failwith
                          ("sell_single: No card pattern."))
                       : card_pattern) in
                let card_pattern : _ =
                  {card_pattern with
                    {quantity =
                       (abs ((SUB (card_pattern.quantity) (1n))))}} in
                let card_patterns : card_patterns =
                  s.card_patterns in
                let card_patterns : _ =
                  Map.add
                    card_pattern
                    card.card_pattern
                    card_patterns in
                let s : _ =
                  {s with
                    {card_patterns = card_patterns}} in
                let cards : cards = s.cards in
                let cards : _ =
                  (Map.remove (action.card_to_sell) (cards)) in
                let s : _ = {s with {cards = cards}} in
                let price : tez =
                  (TIMES
                     (card_pattern.coefficient)
                     (card_pattern.quantity)) in
                let receiver : unit contract =
                  match ((Tezos.get_contract_opt (Tezos.sender))
                         : unit contract option)
                  with
                    Some contract -> contract
                  | None ->
                      ((failwith ("sell_single: No contract."))
                       : unit contract) in
                let op : operation =
                  (Tezos.transaction (unit) (price) (receiver)) in
                let operations : operation list = [op] in
                operations, s
              end
              : return))

    let buy_single : action_buy_single * storage -> return =
      (fun gen__parameters2 : action_buy_single * storage ->
         match gen__parameters2 with
         action : action_buy_single, s : storage ->
             (let card_pattern : card_pattern =
                match Map.find_opt
                        action.card_to_buy
                        s.card_patterns
                with
                  Some pattern -> pattern
                | None ->
                    ((failwith ("buy_single: No card pattern."))
                     : card_pattern) in
              let price : tez =
                (TIMES
                   (card_pattern.coefficient)
                   ((ADD (card_pattern.quantity) (1n)))) in
              begin
                if (GT (price) (Tezos.amount))
                then (failwith ("Not enough money"))
                else ();
                let card_pattern : _ =
                  {card_pattern with
                    {quantity =
                       (ADD (card_pattern.quantity) (1n))}} in
                let card_patterns : card_patterns =
                  s.card_patterns in
                let card_patterns : _ =
                  Map.add
                    card_pattern
                    action.card_to_buy
                    card_patterns in
                let s : _ =
                  {s with
                    {card_patterns = card_patterns}} in
                let cards : cards = s.cards in
                let cards : _ =
                  Map.add
                    {card_pattern = action.card_to_buy;
                     card_owner = Tezos.sender}
                    s.next_id
                    cards in
                let s : _ = {s with {cards = cards}} in
                let s : _ =
                  {s with
                    {next_id = (ADD (s.next_id) (1n))}} in
                ([] : operation list), s
              end
              : return))

    let main : parameter * storage -> return =
      (fun gen__parameters1 : parameter * storage ->
         match gen__parameters1 with
         action : parameter, s : storage ->
             (match action with
                Buy_single bs -> buy_single bs s
              | Sell_single as -> sell_single as s
              | Transfer_single at -> transfer_single at s
              : return)) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/coase.ligo" ; "reasonligo" ] ;
  [%expect {|
type card_pattern_id = nat;

type card_pattern = {quantity: nat, coefficient: tez };

type card_patterns = map(card_pattern_id, card_pattern);

type card_id = nat;

type card = {
  card_pattern: card_pattern_id,
  card_owner: address
};

type cards = map(card_id, card);

type storage = {next_id: nat, cards, card_patterns };

type return = (list(operation), storage);

type action_buy_single = {card_to_buy: card_pattern_id };

type action_sell_single = {card_to_sell: card_id };

type action_transfer_single = {
  destination: address,
  card_to_transfer: card_id
};

type parameter =
  Transfer_single(action_transfer_single)
| Sell_single(action_sell_single)
| Buy_single(action_buy_single);

let transfer_single
: (action_transfer_single, storage) => return =
  ((gen__parameters4: (action_transfer_single, storage)) =>
     switch(gen__parameters4) {
     | action: action_transfer_single, s: storage =>
         let cards: cards = s.cards;
          let card: card =
            switch(
             Map.find_opt(action.card_to_transfer, cards)) {
            | Somecard => card
            | None =>
                (failwith(("transfer_single: No card.")))
                 : card
            };
          begin
            if ((NEQ((card.card_owner), (Tezos.sender)))) {

              (failwith(("This card doesn't belong to you")))
            } else {
              ()
            };
            let card: _ =
              {...card,
                {card_owner: action.destination }};
            let cards: _ =
              Map.add(card, action.card_to_transfer, cards);
            let s: _ = {...s, {cards: cards }};
            [] : list(operation), s
          end
          : return
     });

let sell_single: (action_sell_single, storage) => return =
  ((gen__parameters3: (action_sell_single, storage)) =>
     switch(gen__parameters3) {
     | action: action_sell_single, s: storage =>
         let card: card =
            switch(
             Map.find_opt(action.card_to_sell, s.cards)) {
            | Somecard => card
            | None =>
                (failwith(("sell_single: No card."))) : card
            };
          begin
            if ((NEQ((card.card_owner), (Tezos.sender)))) {

              (failwith(("This card doesn't belong to you")))
            } else {
              ()
            };
            let card_pattern: card_pattern =
              switch(
               Map.find_opt(card.card_pattern,
                  s.card_patterns)) {
              | Somepattern => pattern
              | None =>
                  (
                    failwith(("sell_single: No card pattern.")))
                   : card_pattern
              };
            let card_pattern: _ =
              {...card_pattern,
                {
                  quantity:
                    (
                     abs(((
                         SUB((card_pattern.quantity), (1n))))))
                }};
            let card_patterns: card_patterns =
              s.card_patterns;
            let card_patterns: _ =

              Map.add(card_pattern,
                 card.card_pattern,
                 card_patterns);
            let s: _ =
              {...s,
                {card_patterns: card_patterns }};
            let cards: cards = s.cards;
            let cards: _ =
              (Map.remove((action.card_to_sell), (cards)));
            let s: _ = {...s, {cards: cards }};
            let price: tez =
              (
               TIMES((card_pattern.coefficient),
                  (card_pattern.quantity)));
            let receiver: contract(unit) =
              switch((Tezos.get_contract_opt((Tezos.sender)))
                : option(contract(unit))) {
              | Somecontract => contract
              | None =>
                  (failwith(("sell_single: No contract.")))
                   : contract(unit)
              };
            let op: operation =
              (
               Tezos.transaction((unit), (price), (receiver)));
            let operations: list(operation) = [op];
            operations, s
          end
          : return
     });

let buy_single: (action_buy_single, storage) => return =
  ((gen__parameters2: (action_buy_single, storage)) =>
     switch(gen__parameters2) {
     | action: action_buy_single, s: storage =>
         let card_pattern: card_pattern =
            switch(
             Map.find_opt(action.card_to_buy,
                s.card_patterns)) {
            | Somepattern => pattern
            | None =>
                (failwith(("buy_single: No card pattern.")))
                 : card_pattern
            };
          let price: tez =
            (
             TIMES((card_pattern.coefficient),
                ((ADD((card_pattern.quantity), (1n))))));
          begin
            if ((GT((price), (Tezos.amount)))) {
              (failwith(("Not enough money")))
            } else {
              ()
            };
            let card_pattern: _ =
              {...card_pattern,
                {
                  quantity:
                    (ADD((card_pattern.quantity), (1n)))
                }};
            let card_patterns: card_patterns =
              s.card_patterns;
            let card_patterns: _ =

              Map.add(card_pattern,
                 action.card_to_buy,
                 card_patterns);
            let s: _ =
              {...s,
                {card_patterns: card_patterns }};
            let cards: cards = s.cards;
            let cards: _ =

              Map.add({
                  card_pattern: action.card_to_buy,
                  card_owner: Tezos.sender
                },
                 s.next_id,
                 cards);
            let s: _ = {...s, {cards: cards }};
            let s: _ =
              {...s,
                {next_id: (ADD((s.next_id), (1n))) }};
            [] : list(operation), s
          end
          : return
     });

let main: (parameter, storage) => return =
  ((gen__parameters1: (parameter, storage)) =>
     switch(gen__parameters1) {
     | action: parameter, s: storage =>
         switch(action) {
          | Buy_single bs => buy_single(bs, s)
          | Sell_single as => sell_single(as, s)
          | Transfer_single at => transfer_single(at, s)
          }
          : return
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/deep_access.ligo" ; "pascaligo" ] ;
  [%expect{|
    type pii is int * int

    type ppi is record [y : pii; x : pii]

    type ppp is ppi * ppi

    function main (const toto : unit) : int is
    block {
      const a : ppp
      = (record [y = (10, 11); x = (0, 1)],
         record [y = (110, 111); x = (100, 101)]);
      a.0.x.0 := 2
    } with a.0.x.0

    function asymetric_tuple_access (const foo : unit) : int is
    block {
      const tuple : int * int * int * int = (0, (1, (2, 3)))
    } with
        ADD
          (ADD (ADD (tuple.0, tuple.1.0), tuple.1.1.0),
           tuple.1.1.1)

    type nested_record_t is
      record [nesty : record [mymap : map (int, string)]]

    function nested_record (const nee : nested_record_t)
      : string is
    block {
      nee.nesty.mymap [1] := "one"
    } with
        case nee.nesty.mymap [1] of [
          Some (s) -> s
        | None -> (failwith ("Should not happen.") : string)
        ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/deep_access.ligo" ; "cameligo" ] ;
  [%expect{|
    type pii = int * int

    type ppi = {y : pii; x : pii}

    type ppp = ppi * ppi

    let main : unit -> int =
      (fun toto : unit ->
         let a : ppp =
           {y = 10, 11; x = 0, 1}, {y = 110, 111; x = 100, 101} in
         let a : _ = {a with {0.x.0 = 2}} in
         a.0.x.0)

    let asymetric_tuple_access : unit -> int =
      (fun foo : unit ->
         let tuple : int * int * int * int = 0, 1, 2, 3 in
         (ADD
            ((ADD ((ADD (tuple.0) (tuple.1.0))) (tuple.1.1.0)))
            (tuple.1.1.1)))

    type nested_record_t = {nesty : {mymap : (int, string) map}}

    let nested_record : nested_record_t -> string =
      (fun nee : nested_record_t ->
         let nee : _ = Map.add "one" 1 nesty.mymap in
         match Map.find_opt 1 nee.nesty.mymap with
           Some s -> s
         | None -> ((failwith ("Should not happen.")) : string)) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/deep_access.ligo" ; "reasonligo" ] ;
  [%expect{|
type pii = (int, int);

type ppi = {y: pii, x: pii };

type ppp = (ppi, ppi);

let main: unit => int =
  ((toto: unit): int =>
     let a: ppp =
       {
          y: 10, 11,
          x: 0, 1
        }, {y: 110, 111, x: 100, 101 };
     let a: _ = {...a, {0.x[0]: 2 }};
     a[0].x[0]);

let asymetric_tuple_access: unit => int =
  ((foo: unit): int =>
     let tuple: (int, (int, (int, int))) = 0, 1, 2, 3;
     (
      ADD(((
          ADD(((ADD((tuple[0]), (tuple[1][0])))),
             (tuple[1][1][0])))),
         (tuple[1][1][1]))));

type nested_record_t = {nesty: {mymap: map(int, string) } };

let nested_record: nested_record_t => string =
  ((nee: nested_record_t): string =>
     let nee: _ = Map.add("one", 1, nesty.mymap);
     switch(Map.find_opt(1, nee.nesty.mymap)) {
     | Somes => s
     | None => (failwith(("Should not happen."))) : string
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/double_fold_converter.religo" ; "pascaligo" ] ;
  [%expect{|
type tokenId is nat

type tokenOwner is address

type tokenAmount is nat

type transferContents is
  record [
    token_id : tokenId;
    to_ : tokenOwner;
    amount : tokenAmount
  ]

type transfer is
  record [txs : list (transferContents); from_ : tokenOwner]

type transferContentsMichelson is
  michelson_pair_right_comb (transferContents)

type transferAuxiliary is
  record [
    txs : list (transferContentsMichelson);
    from_ : tokenOwner
  ]

type transferMichelson is
  michelson_pair_right_comb (transferAuxiliary)

type transferParameter is list (transferMichelson)

type parameter is Transfer of transferParameter

type storage is big_map (tokenId, tokenOwner)

type entrypointParameter is parameter * storage

type entrypointReturn is list (operation) * storage

const errorTokenUndefined : _ = "TOKEN_UNDEFINED"

const errorNotOwner : _ = "NOT_OWNER"

const errorInsufficientBalance : _ = "INSUFFICIENT_BALANCE"

type transferContentsIteratorAccumulator is
  storage * tokenOwner

function transferContentsIterator
  (const gen__1 :
     transferContentsIteratorAccumulator *
     transferContentsMichelson)
  : transferContentsIteratorAccumulator is
block {
  const accumulator : _ = gen__1.0;
  const transferContentsMichelson : _ = gen__1.1;
  const gen__2 : _ = accumulator;
  const storage : _ = gen__2.0;
  const from_ : _ = gen__2.1;
  const transferContents : transferContents
  = (Layout.convert_from_right_comb
       (transferContentsMichelson)
     : transferContents);
  const tokenOwner : option (tokenOwner)
  = (Map.find_opt (transferContents.token_id, storage)
     : option (tokenOwner));
  const tokenOwner : _
  = case tokenOwner of [
      Some (tokenOwner) ->
        if EQ (tokenOwner, from_)
        then tokenOwner
        else
          (failwith (errorInsufficientBalance) : tokenOwner)
    | None -> (failwith (errorTokenUndefined) : tokenOwner)
    ];
  const storage : _
  = Map.update
      (transferContents.token_id,
       Some (transferContents.to_), storage)
} with (storage, from_)

function allowOnlyOwnTransfer (const from : tokenOwner)
  : unit is
  if NEQ (from, Tezos.sender)
  then failwith (errorNotOwner)
  else Unit

function transferIterator
  (const gen__3 : storage * transferMichelson) : storage is
block {
  const storage : _ = gen__3.0;
  const transferMichelson : _ = gen__3.1;
  const transferAuxiliary2 : transferAuxiliary
  = (Layout.convert_from_right_comb (transferMichelson)
     : transferAuxiliary);
  const from_ : tokenOwner
  = (transferAuxiliary2.from_ : tokenOwner);
  allowOnlyOwnTransfer (from_);
  const gen__5 : _
  = List.fold
      (transferContentsIterator, transferAuxiliary2.txs,
       (storage, from_));
  const storage : _ = gen__5.0;
  const gen__4 : _ = gen__5.1
} with storage

function transfer
  (const gen__6 : transferParameter * storage)
  : entrypointReturn is
block {
  const transferParameter : _ = gen__6.0;
  const storage : _ = gen__6.1;
  const storage : _
  = List.fold (transferIterator, transferParameter, storage)
} with ((list [] : list (operation)), storage)

function main (const gen__7 : entrypointParameter)
  : entrypointReturn is
block {
  const parameter : _ = gen__7.0;
  const storage : _ = gen__7.1
} with
    case parameter of [
      Transfer (transferParameter) ->
        transfer (transferParameter, storage)
    ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/double_fold_converter.religo" ; "cameligo" ] ;
  [%expect{|
type tokenId = nat

type tokenOwner = address

type tokenAmount = nat

type transferContents =
  {token_id : tokenId;
   to_ : tokenOwner;
   amount : tokenAmount}

type transfer =
  {txs : transferContents list; from_ : tokenOwner}

type transferContentsMichelson =
  transferContents michelson_pair_right_comb

type transferAuxiliary =
  {txs : transferContentsMichelson list; from_ : tokenOwner}

type transferMichelson =
  transferAuxiliary michelson_pair_right_comb

type transferParameter = transferMichelson list

type parameter = Transfer of transferParameter

type storage = (tokenId, tokenOwner) big_map

type entrypointParameter = parameter * storage

type entrypointReturn = operation list * storage

let errorTokenUndefined : _ = "TOKEN_UNDEFINED"

let errorNotOwner : _ = "NOT_OWNER"

let errorInsufficientBalance : _ = "INSUFFICIENT_BALANCE"

type transferContentsIteratorAccumulator =
  storage * tokenOwner

let transferContentsIterator : _ =
  (fun gen__1 :
       transferContentsIteratorAccumulator *
       transferContentsMichelson ->
     let accumulator : _ = gen__1.0 in
     let transferContentsMichelson : _ = gen__1.1 in
     let gen__2 : _ = accumulator in
     let storage : _ = gen__2.0 in
     let from_ : _ = gen__2.1 in
     let transferContents : transferContents =
       ((Layout.convert_from_right_comb
           (transferContentsMichelson))
        : transferContents) in
     let tokenOwner : tokenOwner option =
       ((Map.find_opt (transferContents.token_id) (storage))
        : tokenOwner option) in
     let tokenOwner : _ =
       match tokenOwner with
         Some tokenOwner ->
           if (EQ (tokenOwner) (from_))
           then tokenOwner
           else
             ((failwith (errorInsufficientBalance))
              : tokenOwner)
       | None ->
           ((failwith (errorTokenUndefined)) : tokenOwner) in
     let storage : _ =
       (Map.update
          (transferContents.token_id)
          ((Some (transferContents.to_)))
          (storage)) in
     storage, from_)

let allowOnlyOwnTransfer : _ =
  (fun from : tokenOwner ->
     if (NEQ (from) (Tezos.sender))
     then (failwith (errorNotOwner))
     else ())

let transferIterator : _ =
  (fun gen__3 : storage * transferMichelson ->
     let storage : _ = gen__3.0 in
     let transferMichelson : _ = gen__3.1 in
     let transferAuxiliary2 : transferAuxiliary =
       ((Layout.convert_from_right_comb (transferMichelson))
        : transferAuxiliary) in
     let from_ : tokenOwner =
       (transferAuxiliary2.from_ : tokenOwner) in
     begin
       allowOnlyOwnTransfer from_;
       let gen__5 : _ =
         (List.fold
            (transferContentsIterator)
            (transferAuxiliary2.txs)
            (storage, from_)) in
       let storage : _ = gen__5.0 in
       let gen__4 : _ = gen__5.1 in
       storage
     end)

let transfer : _ =
  (fun gen__6 : transferParameter * storage ->
     let transferParameter : _ = gen__6.0 in
     let storage : _ = gen__6.1 in
     let storage : _ =
       (List.fold
          (transferIterator)
          (transferParameter)
          (storage)) in
     ([] : operation list), storage)

let main : _ =
  (fun gen__7 : entrypointParameter ->
     let parameter : _ = gen__7.0 in
     let storage : _ = gen__7.1 in
     match parameter with
     Transfer transferParameter ->
         transfer transferParameter storage) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/double_fold_converter.religo" ; "reasonligo" ] ;
  [%expect{|
type tokenId = nat;

type tokenOwner = address;

type tokenAmount = nat;

type transferContents = {
  token_id: tokenId,
  to_: tokenOwner,
  amount: tokenAmount
};

type transfer = {
  txs: list(transferContents),
  from_: tokenOwner
};

type transferContentsMichelson = michelson_pair_right_comb
  (transferContents);

type transferAuxiliary = {
  txs: list(transferContentsMichelson),
  from_: tokenOwner
};

type transferMichelson = michelson_pair_right_comb
  (transferAuxiliary);

type transferParameter = list(transferMichelson);

type parameter = Transfer(transferParameter);

type storage = big_map(tokenId, tokenOwner);

type entrypointParameter = (parameter, storage);

type entrypointReturn = (list(operation), storage);

let errorTokenUndefined: _ = "TOKEN_UNDEFINED";

let errorNotOwner: _ = "NOT_OWNER";

let errorInsufficientBalance: _ = "INSUFFICIENT_BALANCE";

type transferContentsIteratorAccumulator = (storage,
 tokenOwner);

let transferContentsIterator: _ =
  ((gen__1: (transferContentsIteratorAccumulator,
      transferContentsMichelson))
   : transferContentsIteratorAccumulator =>
     let accumulator: _ = gen__1[0];
     let transferContentsMichelson: _ = gen__1[1];
     let gen__2: _ = accumulator;
     let storage: _ = gen__2[0];
     let from_: _ = gen__2[1];
     let transferContents: transferContents =
       (
         Layout.convert_from_right_comb((transferContentsMichelson)))
        : transferContents;
     let tokenOwner: option(tokenOwner) =
       (Map.find_opt((transferContents.token_id), (storage)))
        : option(tokenOwner);
     let tokenOwner: _ =
       switch(tokenOwner) {
       | SometokenOwner =>
           if ((EQ((tokenOwner), (from_)))) {
             tokenOwner
           } else {

             (failwith((errorInsufficientBalance)))
              : tokenOwner
           }
       | None =>
           (failwith((errorTokenUndefined))) : tokenOwner
       };
     let storage: _ =
       (
        Map.update((transferContents.token_id),
           ((Some((transferContents.to_)))),
           (storage)));
     storage, from_);

let allowOnlyOwnTransfer: _ =
  ((from: tokenOwner): unit =>
     if ((NEQ((from), (Tezos.sender)))) {
       (failwith((errorNotOwner)))
     } else {
       ()
     });

let transferIterator: _ =
  ((gen__3: (storage, transferMichelson)): storage =>
     let storage: _ = gen__3[0];
     let transferMichelson: _ = gen__3[1];
     let transferAuxiliary2: transferAuxiliary =
       (Layout.convert_from_right_comb((transferMichelson)))
        : transferAuxiliary;
     let from_: tokenOwner =
       transferAuxiliary2.from_ : tokenOwner;
     begin
       allowOnlyOwnTransfer(from_);
       let gen__5: _ =
         (
          List.fold((transferContentsIterator),
             (transferAuxiliary2.txs),
             (storage, from_)));
       let storage: _ = gen__5[0];
       let gen__4: _ = gen__5[1];
       storage
     end);

let transfer: _ =
  ((gen__6: (transferParameter, storage)): entrypointReturn =>
     let transferParameter: _ = gen__6[0];
     let storage: _ = gen__6[1];
     let storage: _ =
       (
        List.fold((transferIterator),
           (transferParameter),
           (storage)));
     [] : list(operation), storage);

let main: _ =
  ((gen__7: entrypointParameter): entrypointReturn =>
     let parameter: _ = gen__7[0];
     let storage: _ = gen__7[1];
     switch(parameter) {
     | Transfer transferParameter =>
         transfer(transferParameter, storage)
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/FA1.2.ligo" ; "pascaligo" ] ;
  [%expect {|
    type tokens is big_map (address, nat)

    type allowances is big_map (address * address, nat)

    type storage is
      record [
        total_amount : nat;
        tokens : tokens;
        allowances : allowances
      ]

    type transfer is
      record [
        value : nat;
        address_to : address;
        address_from : address
      ]

    type approve is record [value : nat; spender : address]

    type getAllowance is
      record [
        spender : address;
        owner : address;
        callback : contract (nat)
      ]

    type getBalance is
      record [owner : address; callback : contract (nat)]

    type getTotalSupply is record [callback : contract (nat)]

    type action is
        Transfer of transfer | GetTotalSupply of getTotalSupply
      | GetBalance of getBalance | GetAllowance of getAllowance
      | Approve of approve

    function transfer
      (const gen__parameters6 : transfer * storage) is
      case gen__parameters6 of [
        (p, s) ->
          (block {
             const new_allowances : allowances = big_map [];
             const gen__env9 : _
             = record [new_allowances = new_allowances];
             const gen__env9 : _
             = if EQ (Tezos.sender, p.address_from)
               then
                 block {
                   const new_allowances : _ = s.allowances;
                   gen__env9.new_allowances := new_allowances;
                   skip
                 } with gen__env9
               else
                 block {
                   const authorized_value : nat
                   = case Map.find_opt
                            ((Tezos.sender, p.address_from),
                             s.allowances)
                     of [
                       Some (value) -> value
                     | None -> 0n
                     ];
                   const gen__env8 : _
                   = record [new_allowances = new_allowances];
                   const gen__env8 : _
                   = if LT (authorized_value, p.value)
                     then
                       block {
                         failwith ("Not Enough Allowance")
                       } with gen__env8
                     else
                       block {
                         const new_allowances : _
                         = Map.update
                             ((Tezos.sender, p.address_from),
                              Some
                                (abs
                                   (SUB
                                      (authorized_value, p.value))),
                              s.allowances);
                         gen__env8.new_allowances :=
                           new_allowances;
                         skip
                       } with gen__env8;
                   const new_allowances : _
                   = gen__env8.new_allowances;
                   gen__env9.new_allowances := new_allowances;
                   skip
                 } with gen__env9;
             const new_allowances : _ = gen__env9.new_allowances;
             const sender_balance : nat
             = case Map.find_opt (p.address_from, s.tokens) of [
                 Some (value) -> value
               | None -> 0n
               ];
             const new_tokens : tokens = big_map [];
             const gen__env12 : _
             = record [new_tokens = new_tokens];
             const gen__env12 : _
             = if LT (sender_balance, p.value)
               then
                 block {
                   failwith ("Not Enough Balance")
                 } with gen__env12
               else
                 block {
                   const new_tokens : _
                   = Map.update
                       (p.address_from,
                        Some
                          (abs (SUB (sender_balance, p.value))),
                        s.tokens);
                   gen__env12.new_tokens := new_tokens;
                   const receiver_balance : nat
                   = case Map.find_opt (p.address_to, s.tokens)
                     of [
                       Some (value) -> value
                     | None -> 0n
                     ];
                   const new_tokens : _
                   = Map.update
                       (p.address_to,
                        Some (ADD (receiver_balance, p.value)),
                        new_tokens);
                   gen__env12.new_tokens := new_tokens;
                   skip
                 } with gen__env12;
             const new_tokens : _ = gen__env12.new_tokens
           } with
               ((list [] : list (operation)),
                s with
                  record [
                    allowances = new_allowances;
                    tokens = new_tokens
                  ])
           : list (operation) * storage)
      ]

    function approve
      (const gen__parameters5 : approve * storage) is
      case gen__parameters5 of [
        (p, s) ->
          (block {
             const previous_value : nat
             = case Map.find_opt
                      ((p.spender, Tezos.sender), s.allowances)
               of [
                 Some (value) -> value
               | None -> 0n
               ];
             const new_allowances : allowances = big_map [];
             const gen__env14 : _
             = record [new_allowances = new_allowances];
             const gen__env14 : _
             = if AND
                    (GT (previous_value, 0n), GT (p.value, 0n))
               then
                 block {
                   failwith ("Unsafe Allowance Change")
                 } with gen__env14
               else
                 block {
                   const new_allowances : _
                   = Map.update
                       ((p.spender, Tezos.sender),
                        Some (p.value), s.allowances);
                   gen__env14.new_allowances := new_allowances;
                   skip
                 } with gen__env14;
             const new_allowances : _
             = gen__env14.new_allowances
           } with
               ((list [] : list (operation)),
                s with
                  record [allowances = new_allowances])
           : list (operation) * storage)
      ]

    function getAllowance
      (const gen__parameters4 : getAllowance * storage) is
      case gen__parameters4 of [
        (p, s) ->
          (block {
             const value : nat
             = case Map.find_opt
                      ((p.owner, p.spender), s.allowances)
               of [
                 Some (value) -> value
               | None -> 0n
               ];
             const op : operation
             = Tezos.transaction (value, 0mutez, p.callback)
           } with (list [op], s)
           : list (operation) * storage)
      ]

    function getBalance
      (const gen__parameters3 : getBalance * storage) is
      case gen__parameters3 of [
        (p, s) ->
          (block {
             const value : nat
             = case Map.find_opt (p.owner, s.tokens) of [
                 Some (value) -> value
               | None -> 0n
               ];
             const op : operation
             = Tezos.transaction (value, 0mutez, p.callback)
           } with (list [op], s)
           : list (operation) * storage)
      ]

    function getTotalSupply
      (const gen__parameters2 : getTotalSupply * storage) is
      case gen__parameters2 of [
        (p, s) ->
          (block {
             const total : nat = s.total_amount;
             const op : operation
             = Tezos.transaction (total, 0mutez, p.callback)
           } with (list [op], s)
           : list (operation) * storage)
      ]

    function main (const gen__parameters1 : action * storage) is
      case gen__parameters1 of [
        (a, s) ->
          (case a of [
             Transfer (p) -> transfer (p, s)
           | Approve (p) -> approve (p, s)
           | GetAllowance (p) -> getAllowance (p, s)
           | GetBalance (p) -> getBalance (p, s)
           | GetTotalSupply (p) -> getTotalSupply (p, s)
           ]
           : list (operation) * storage)
      ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/FA1.2.ligo" ; "cameligo" ] ;
  [%expect {|
    type tokens = (address, nat) big_map

    type allowances = (address * address, nat) big_map

    type storage =
      {total_amount : nat;
       tokens : tokens;
       allowances : allowances}

    type transfer =
      {value : nat;
       address_to : address;
       address_from : address}

    type approve = {value : nat; spender : address}

    type getAllowance =
      {spender : address;
       owner : address;
       callback : nat contract}

    type getBalance = {owner : address; callback : nat contract}

    type getTotalSupply = {callback : nat contract}

    type action =
      Transfer of transfer | GetTotalSupply of getTotalSupply
    | GetBalance of getBalance | GetAllowance of getAllowance
    | Approve of approve

    let transfer
    : transfer * storage -> operation list * storage =
      (fun gen__parameters6 : transfer * storage ->
         match gen__parameters6 with
         p : transfer, s : storage ->
             (let new_allowances : allowances = Big_map.empty in
              let gen__env9 : _ =
                {new_allowances = new_allowances} in
              let gen__env9 : _ =
                if (EQ (Tezos.sender) (p.address_from))
                then
                  let new_allowances : _ = s.allowances in
                  let gen__env9 : _ =
                    {gen__env9 with
                      {new_allowances = new_allowances}} in
                  begin
                    ();
                    gen__env9
                  end
                else
                  let authorized_value : nat =
                    match (Map.find_opt
                             (Tezos.sender, p.address_from)
                             (s.allowances))
                    with
                      Some value -> value
                    | None -> 0n in
                  let gen__env8 : _ =
                    {new_allowances = new_allowances} in
                  let gen__env8 : _ =
                    if (LT (authorized_value) (p.value))
                    then
                      begin
                        (failwith ("Not Enough Allowance"));
                        gen__env8
                      end
                    else
                      let new_allowances : _ =
                        (Map.update
                           (Tezos.sender, p.address_from)
                           ((Some
                               ((abs
                                   ((SUB
                                       (authorized_value)
                                       (p.value)))))))
                           (s.allowances)) in
                      let gen__env8 : _ =
                        {gen__env8 with
                          {new_allowances = new_allowances}} in
                      begin
                        ();
                        gen__env8
                      end in
                  let new_allowances : _ =
                    gen__env8.new_allowances in
                  let gen__env9 : _ =
                    {gen__env9 with
                      {new_allowances = new_allowances}} in
                  begin
                    ();
                    gen__env9
                  end in
              let new_allowances : _ = gen__env9.new_allowances in
              let sender_balance : nat =
                match (Map.find_opt (p.address_from) (s.tokens))
                with
                  Some value -> value
                | None -> 0n in
              let new_tokens : tokens = Big_map.empty in
              let gen__env12 : _ = {new_tokens = new_tokens} in
              let gen__env12 : _ =
                if (LT (sender_balance) (p.value))
                then
                  begin
                    (failwith ("Not Enough Balance"));
                    gen__env12
                  end
                else
                  let new_tokens : _ =
                    (Map.update
                       (p.address_from)
                       ((Some
                           ((abs
                               ((SUB (sender_balance) (p.value)))))))
                       (s.tokens)) in
                  let gen__env12 : _ =
                    {gen__env12 with
                      {new_tokens = new_tokens}} in
                  let receiver_balance : nat =
                    match (Map.find_opt
                             (p.address_to)
                             (s.tokens))
                    with
                      Some value -> value
                    | None -> 0n in
                  let new_tokens : _ =
                    (Map.update
                       (p.address_to)
                       ((Some
                           ((ADD (receiver_balance) (p.value)))))
                       (new_tokens)) in
                  let gen__env12 : _ =
                    {gen__env12 with
                      {new_tokens = new_tokens}} in
                  begin
                    ();
                    gen__env12
                  end in
              let new_tokens : _ = gen__env12.new_tokens in
              ([] : operation list),
              {s with
                {allowances = new_allowances;
                 tokens = new_tokens}}
              : operation list * storage))

    let approve : approve * storage -> operation list * storage =
      (fun gen__parameters5 : approve * storage ->
         match gen__parameters5 with
         p : approve, s : storage ->
             (let previous_value : nat =
                match (Map.find_opt
                         (p.spender, Tezos.sender)
                         (s.allowances))
                with
                  Some value -> value
                | None -> 0n in
              let new_allowances : allowances = Big_map.empty in
              let gen__env14 : _ =
                {new_allowances = new_allowances} in
              let gen__env14 : _ =
                if (AND
                      ((GT (previous_value) (0n)))
                      ((GT (p.value) (0n))))
                then
                  begin
                    (failwith ("Unsafe Allowance Change"));
                    gen__env14
                  end
                else
                  let new_allowances : _ =
                    (Map.update
                       (p.spender, Tezos.sender)
                       ((Some (p.value)))
                       (s.allowances)) in
                  let gen__env14 : _ =
                    {gen__env14 with
                      {new_allowances = new_allowances}} in
                  begin
                    ();
                    gen__env14
                  end in
              let new_allowances : _ = gen__env14.new_allowances in
              ([] : operation list),
              {s with
                {allowances = new_allowances}}
              : operation list * storage))

    let getAllowance
    : getAllowance * storage -> operation list * storage =
      (fun gen__parameters4 : getAllowance * storage ->
         match gen__parameters4 with
         p : getAllowance, s : storage ->
             (let value : nat =
                match (Map.find_opt
                         (p.owner, p.spender)
                         (s.allowances))
                with
                  Some value -> value
                | None -> 0n in
              let op : operation =
                (Tezos.transaction (value) (0mutez) (p.callback)) in
              [op], s
              : operation list * storage))

    let getBalance
    : getBalance * storage -> operation list * storage =
      (fun gen__parameters3 : getBalance * storage ->
         match gen__parameters3 with
         p : getBalance, s : storage ->
             (let value : nat =
                match (Map.find_opt (p.owner) (s.tokens)) with
                  Some value -> value
                | None -> 0n in
              let op : operation =
                (Tezos.transaction (value) (0mutez) (p.callback)) in
              [op], s
              : operation list * storage))

    let getTotalSupply
    : getTotalSupply * storage -> operation list * storage =
      (fun gen__parameters2 : getTotalSupply * storage ->
         match gen__parameters2 with
         p : getTotalSupply, s : storage ->
             (let total : nat = s.total_amount in
              let op : operation =
                (Tezos.transaction (total) (0mutez) (p.callback)) in
              [op], s
              : operation list * storage))

    let main : action * storage -> operation list * storage =
      (fun gen__parameters1 : action * storage ->
         match gen__parameters1 with
         a : action, s : storage ->
             (match a with
                Transfer p -> transfer p s
              | Approve p -> approve p s
              | GetAllowance p -> getAllowance p s
              | GetBalance p -> getBalance p s
              | GetTotalSupply p -> getTotalSupply p s
              : operation list * storage)) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/FA1.2.ligo" ; "reasonligo" ] ;
  [%expect {|
type tokens = big_map(address, nat);

type allowances = big_map((address, address), nat);

type storage = {total_amount: nat, tokens, allowances };

type transfer = {
  value: nat,
  address_to: address,
  address_from: address
};

type approve = {value: nat, spender: address };

type getAllowance = {
  spender: address,
  owner: address,
  callback: contract(nat)
};

type getBalance = {owner: address, callback: contract(nat) };

type getTotalSupply = {callback: contract(nat) };

type action =
  Transfer(transfer)
| GetTotalSupply(getTotalSupply)
| GetBalance(getBalance)
| GetAllowance(getAllowance)
| Approve(approve);

let transfer
: (transfer, storage) => (list(operation), storage) =
  ((gen__parameters6: (transfer, storage)) =>
     switch(gen__parameters6) {
     | p: transfer, s: storage =>
         let new_allowances: allowances = Big_map.empty;
          let gen__env9: _ = {
            new_allowances: new_allowances
          };
          let gen__env9: _ =
            if ((EQ((Tezos.sender), (p.address_from)))) {

              let new_allowances: _ = s.allowances;
              let gen__env9: _ =
                {...gen__env9,
                  {new_allowances: new_allowances }};
              begin
                ();
                gen__env9
              end
            } else {

              let authorized_value: nat =
                switch((
                  Map.find_opt((Tezos.sender, p.address_from),
                     (s.allowances)))) {
                | Somevalue => value
                | None => 0n
                };
              let gen__env8: _ = {
                new_allowances: new_allowances
              };
              let gen__env8: _ =
                if ((LT((authorized_value), (p.value)))) {

                  begin
                    (failwith(("Not Enough Allowance")));
                    gen__env8
                  end
                } else {

                  let new_allowances: _ =
                    (
                     Map.update((Tezos.sender,
                         p.address_from),
                        ((
                         Some(((
                             abs(((
                                 SUB((authorized_value),
                                    (p.value)))))))))),
                        (s.allowances)));
                  let gen__env8: _ =
                    {...gen__env8,
                      {new_allowances: new_allowances }};
                  begin
                    ();
                    gen__env8
                  end
                };
              let new_allowances: _ =
                gen__env8.new_allowances;
              let gen__env9: _ =
                {...gen__env9,
                  {new_allowances: new_allowances }};
              begin
                ();
                gen__env9
              end
            };
          let new_allowances: _ = gen__env9.new_allowances;
          let sender_balance: nat =
            switch((
              Map.find_opt((p.address_from), (s.tokens)))) {
            | Somevalue => value
            | None => 0n
            };
          let new_tokens: tokens = Big_map.empty;
          let gen__env12: _ = {
            new_tokens: new_tokens
          };
          let gen__env12: _ =
            if ((LT((sender_balance), (p.value)))) {

              begin
                (failwith(("Not Enough Balance")));
                gen__env12
              end
            } else {

              let new_tokens: _ =
                (
                 Map.update((p.address_from),
                    ((
                     Some(((
                         abs(((
                             SUB((sender_balance), (p.value)))))))))),
                    (s.tokens)));
              let gen__env12: _ =
                {...gen__env12,
                  {new_tokens: new_tokens }};
              let receiver_balance: nat =
                switch((
                  Map.find_opt((p.address_to), (s.tokens)))) {
                | Somevalue => value
                | None => 0n
                };
              let new_tokens: _ =
                (
                 Map.update((p.address_to),
                    ((
                     Some(((
                         ADD((receiver_balance), (p.value))))))),
                    (new_tokens)));
              let gen__env12: _ =
                {...gen__env12,
                  {new_tokens: new_tokens }};
              begin
                ();
                gen__env12
              end
            };
          let new_tokens: _ = gen__env12.new_tokens;
          [] : list(operation),
           {...s,
             {
               allowances: new_allowances,
               tokens: new_tokens
             }}
          : (list(operation), storage)
     });

let approve
: (approve, storage) => (list(operation), storage) =
  ((gen__parameters5: (approve, storage)) =>
     switch(gen__parameters5) {
     | p: approve, s: storage =>
         let previous_value: nat =
            switch((
              Map.find_opt((p.spender, Tezos.sender),
                 (s.allowances)))) {
            | Somevalue => value
            | None => 0n
            };
          let new_allowances: allowances = Big_map.empty;
          let gen__env14: _ = {
            new_allowances: new_allowances
          };
          let gen__env14: _ =
            if ((
             AND(((GT((previous_value), (0n)))),
                ((GT((p.value), (0n))))))) {

              begin
                (failwith(("Unsafe Allowance Change")));
                gen__env14
              end
            } else {

              let new_allowances: _ =
                (
                 Map.update((p.spender, Tezos.sender),
                    ((Some((p.value)))),
                    (s.allowances)));
              let gen__env14: _ =
                {...gen__env14,
                  {new_allowances: new_allowances }};
              begin
                ();
                gen__env14
              end
            };
          let new_allowances: _ = gen__env14.new_allowances;
          [] : list(operation),
           {...s,
             {allowances: new_allowances }}
          : (list(operation), storage)
     });

let getAllowance
: (getAllowance, storage) => (list(operation), storage) =
  ((gen__parameters4: (getAllowance, storage)) =>
     switch(gen__parameters4) {
     | p: getAllowance, s: storage =>
         let value: nat =
            switch((
              Map.find_opt((p.owner, p.spender),
                 (s.allowances)))) {
            | Somevalue => value
            | None => 0n
            };
          let op: operation =
            (
             Tezos.transaction((value),
                (0mutez),
                (p.callback)));
          [op], s
          : (list(operation), storage)
     });

let getBalance
: (getBalance, storage) => (list(operation), storage) =
  ((gen__parameters3: (getBalance, storage)) =>
     switch(gen__parameters3) {
     | p: getBalance, s: storage =>
         let value: nat =
            switch((Map.find_opt((p.owner), (s.tokens)))) {
            | Somevalue => value
            | None => 0n
            };
          let op: operation =
            (
             Tezos.transaction((value),
                (0mutez),
                (p.callback)));
          [op], s
          : (list(operation), storage)
     });

let getTotalSupply
: (getTotalSupply, storage) => (list(operation), storage) =
  ((gen__parameters2: (getTotalSupply, storage)) =>
     switch(gen__parameters2) {
     | p: getTotalSupply, s: storage =>
         let total: nat = s.total_amount;
          let op: operation =
            (
             Tezos.transaction((total),
                (0mutez),
                (p.callback)));
          [op], s
          : (list(operation), storage)
     });

let main: (action, storage) => (list(operation), storage) =
  ((gen__parameters1: (action, storage)) =>
     switch(gen__parameters1) {
     | a: action, s: storage =>
         switch(a) {
          | Transfer p => transfer(p, s)
          | Approve p => approve(p, s)
          | GetAllowance p => getAllowance(p, s)
          | GetBalance p => getBalance(p, s)
          | GetTotalSupply p => getTotalSupply(p, s)
          }
          : (list(operation), storage)
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/failwith.ligo" ; "pascaligo" ] ;
  [%expect {|
    type parameter is Zero of nat | Pos of nat

    type storage is unit

    type return is list (operation) * storage

    function main (const gen__parameters1 : parameter * storage) is
      case gen__parameters1 of [
        (p, s) ->
          (block {
             case p of [
               Zero (n) ->
                 if GT (n, 0n) then failwith ("fail") else skip
             | Pos (n) ->
                 if GT (n, 0n) then skip else failwith ("fail")
             ]
           } with ((list [] : list (operation)), s)
           : return)
      ]

    function foobar (const i : int) : int is
    block {
      const p : parameter = (Zero (42n));
      const gen__env7 : _ = record [i = i];
      const gen__env7 : _
      = if GT (i, 0)
        then
          block {
            const i : _ = ADD (i, 1);
            gen__env7.i := i;
            const gen__env5 : _ = record [i = i];
            const gen__env5 : _
            = if GT (i, 10)
              then
                block {
                  const i : _ = 20;
                  gen__env5.i := i;
                  failwith ("who knows");
                  const i : _ = 30;
                  gen__env5.i := i;
                  skip
                } with gen__env5
              else
                block {
                  skip
                } with gen__env5;
            const i : _ = gen__env5.i;
            gen__env7.i := i;
            skip
          } with gen__env7
        else
          block {
            case p of [
              Zero (n) -> failwith (42n)
            | Pos (n) -> skip
            ]
          } with gen__env7;
      const i : _ = gen__env7.i
    } with
        case p of [
          Zero (n) -> i
        | Pos (n) -> (failwith ("waaaa") : int)
        ]

    function failer (const p : int) : int is
    block {
      if EQ (p, 1) then failwith (42) else skip
    } with p |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/failwith.ligo" ; "cameligo" ] ;
  [%expect {|
    type parameter = Zero of nat | Pos of nat

    type storage = unit

    type return = operation list * storage

    let main : parameter * storage -> return =
      (fun gen__parameters1 : parameter * storage ->
         match gen__parameters1 with
         p : parameter, s : storage ->
             (begin
                match p with
                  Zero n ->
                    if (GT (n) (0n))
                    then (failwith ("fail"))
                    else ()
                | Pos n ->
                    if (GT (n) (0n))
                    then ()
                    else (failwith ("fail"));
                ([] : operation list), s
              end
              : return))

    let foobar : int -> int =
      (fun i : int ->
         let p : parameter = (Zero 42n) in
         let gen__env7 : _ = {i = i} in
         let gen__env7 : _ =
           if (GT (i) (0))
           then
             let i : _ = (ADD (i) (1)) in
             let gen__env7 : _ = {gen__env7 with {i = i}} in
             let gen__env5 : _ = {i = i} in
             let gen__env5 : _ =
               if (GT (i) (10))
               then
                 let i : _ = 20 in
                 let gen__env5 : _ = {gen__env5 with {i = i}} in
                 begin
                   (failwith ("who knows"));
                   let i : _ = 30 in
                   let gen__env5 : _ = {gen__env5 with {i = i}} in
                   begin
                     ();
                     gen__env5
                   end
                 end
               else
                 begin
                   ();
                   gen__env5
                 end in
             let i : _ = gen__env5.i in
             let gen__env7 : _ = {gen__env7 with {i = i}} in
             begin
               ();
               gen__env7
             end
           else
             begin
               match p with
                 Zero n -> (failwith (42n))
               | Pos n -> ();
               gen__env7
             end in
         let i : _ = gen__env7.i in
         match p with
           Zero n -> i
         | Pos n -> ((failwith ("waaaa")) : int))

    let failer : int -> int =
      (fun p : int ->
         begin
           if (EQ (p) (1)) then (failwith (42)) else ();
           p
         end) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/failwith.ligo" ; "reasonligo" ] ;
  [%expect {|
type parameter = Zero(nat) | Pos(nat);

type storage = unit;

type return = (list(operation), storage);

let main: (parameter, storage) => return =
  ((gen__parameters1: (parameter, storage)) =>
     switch(gen__parameters1) {
     | p: parameter, s: storage =>
         begin
            switch(p) {
            | Zero n =>
                if ((GT((n), (0n)))) {
                  (failwith(("fail")))
                } else {
                  ()
                }
            | Pos n =>
                if ((GT((n), (0n)))) {
                  ()
                } else {
                  (failwith(("fail")))
                }
            };
            [] : list(operation), s
          end
          : return
     });

let foobar: int => int =
  ((i: int): int =>
     let p: parameter = (Zero 42n);
     let gen__env7: _ = {
       i: i
     };
     let gen__env7: _ =
       if ((GT((i), (0)))) {

         let i: _ = (ADD((i), (1)));
         let gen__env7: _ = {...gen__env7, {i: i }};
         let gen__env5: _ = {
           i: i
         };
         let gen__env5: _ =
           if ((GT((i), (10)))) {

             let i: _ = 20;
             let gen__env5: _ = {...gen__env5, {i: i }};
             begin
               (failwith(("who knows")));
               let i: _ = 30;
               let gen__env5: _ = {...gen__env5, {i: i }};
               begin
                 ();
                 gen__env5
               end
             end
           } else {

             begin
               ();
               gen__env5
             end
           };
         let i: _ = gen__env5.i;
         let gen__env7: _ = {...gen__env7, {i: i }};
         begin
           ();
           gen__env7
         end
       } else {

         begin
           switch(p) {
           | Zero n => (failwith((42n)))
           | Pos n => ()
           };
           gen__env7
         end
       };
     let i: _ = gen__env7.i;
     switch(p) {
     | Zero n => i
     | Pos n => (failwith(("waaaa"))) : int
     });

let failer: int => int =
  ((p: int): int => begin
     if ((EQ((p), (1)))) {
       (failwith((42)))
     } else {
       ()
     };
     p
   end); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/recursion.ligo" ; "pascaligo" ] ;
  [%expect {|
    recursive function sum (const gen__parameters2 : int * int) is
      case gen__parameters2 of [
        (n, acc) ->
          (if LT (n, 1)
           then acc
           else sum (SUB (n, 1), ADD (acc, n))
           : int)
      ]

    recursive
    function fibo
      (const gen__parameters1 : int * int * int) is
      case gen__parameters1 of [
        (n, n_1, n_0) ->
          (if LT (n, 2)
           then n_1
           else fibo (SUB (n, 1), ADD (n_1, n_0), n_1)
           : int)
      ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/recursion.ligo" ; "cameligo" ] ;
  [%expect {|
    let rec sum : int * int -> int =
      (fun gen__parameters2 : int * int ->
         match gen__parameters2 with
         n : int, acc : int ->
             (if (LT (n) (1))
              then acc
              else sum (SUB (n) (1)) (ADD (acc) (n))
              : int))

    let rec fibo : int * int * int -> int =
      (fun gen__parameters1 : int * int * int ->
         match gen__parameters1 with
         n : int, n_1 : int, n_0 : int ->
             (if (LT (n) (2))
              then n_1
              else fibo (SUB (n) (1)) (ADD (n_1) (n_0)) n_1
              : int)) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/recursion.ligo" ; "reasonligo" ] ;
  [%expect {|
    let rec sum: (int, int) => int =
      ((gen__parameters2: (int, int)) =>
         switch(gen__parameters2) {
         | n: int, acc: int =>
             if ((LT((n), (1)))) {
                acc
              } else {
                sum((SUB((n), (1))), (ADD((acc), (n))))
              }
              : int
         });

    let rec fibo: (int, int, int) => int =
      ((gen__parameters1: (int, int, int)) =>
         switch(gen__parameters1) {
         | n: int, n_1: int, n_0: int =>
             if ((LT((n), (2)))) {
                n_1
              } else {
                fibo((SUB((n), (1))), (ADD((n_1), (n_0))), n_1)
              }
              : int
         }); |}]
