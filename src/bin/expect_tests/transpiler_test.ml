open Cli_expect

let%expect_test _ =
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/coase.ligo" ; "pascaligo" ] ;
  [%expect {|
    type card_pattern_id is nat

    type card_pattern is
      record [coefficient : tez; quantity : nat]

    type card_patterns is map (card_pattern_id, card_pattern)

    type card_id is nat

    type card is
      record [
        card_owner : address;
        card_pattern : card_pattern_id
      ]

    type cards is map (card_id, card)

    type storage is
      record [
        card_patterns : card_patterns;
        cards : cards;
        next_id : nat
      ]

    type return is list (operation) * storage

    type action_buy_single is
      record [card_to_buy : card_pattern_id]

    type action_sell_single is record [card_to_sell : card_id]

    type action_transfer_single is
      record [card_to_transfer : card_id; destination : address]

    type parameter is
        Buy_single of action_buy_single
      | Sell_single of action_sell_single
      | Transfer_single of action_transfer_single

    function transfer_single
      (const gen__parameters2 : action_transfer_single * storage) is
      case gen__parameters2 of [
        (action, s) ->
          {
            const cards : cards = s.cards;

            const card : card
            = case cards [action.card_to_transfer]  of [
                Some (card) -> card
              | None (Unit) ->
                  (failwith ("transfer_single: No card.")
                   : card)
              ];

            if Operator.neq
                 (card.card_owner,
                  Tezos.get_sender (Unit))
            then failwith ("This card doesn't belong to you")
            else skip;

            card := card.card_owner with action.destination;

            cards :=
              Map.add (action.card_to_transfer, card, cards);

            s := s.cards with cards
          } with ((list [] : list (operation)), s)
      ]

    function sell_single
      (const gen__parameters3 : action_sell_single * storage) is
      case gen__parameters3 of [
        (action, s) ->
          {
            const card : card
            = case s.cards [action.card_to_sell]  of [
                Some (card) -> card
              | None (Unit) ->
                  (failwith ("sell_single: No card.") : card)
              ];

            if Operator.neq
                 (card.card_owner,
                  Tezos.get_sender (Unit))
            then failwith ("This card doesn't belong to you")
            else skip;

            const card_pattern : card_pattern
            = case s.card_patterns [card.card_pattern]  of [
                Some (pattern) -> pattern
              | None (Unit) ->
                  (failwith ("sell_single: No card pattern.")
                   : card_pattern)
              ];

            card_pattern :=
              card_pattern.quantity with
                abs (Operator.sub (card_pattern.quantity, 1n));

            const card_patterns : card_patterns
            = s.card_patterns;

            card_patterns :=
              Map.add
                (card.card_pattern,
                 card_pattern,
                 card_patterns);

            s := s.card_patterns with card_patterns;

            const cards : cards = s.cards;

            cards := Map.remove (action.card_to_sell, cards);

            s := s.cards with cards;

            const price : tez
            = Operator.times
                (card_pattern.coefficient,
                 card_pattern.quantity);

            const receiver : contract (unit)
            = case (Tezos.get_contract_opt
                      (Tezos.get_sender (Unit))
                    : option (contract (unit)))
              of [
                Some (contract) -> contract
              | None (Unit) ->
                  (failwith ("sell_single: No contract.")
                   : contract (unit))
              ];

            const op : operation
            = Tezos.transaction (unit, price, receiver);

            const operations : list (operation) = list [op]
          } with (operations, s)
      ]

    function buy_single
      (const gen__parameters4 : action_buy_single * storage) is
      case gen__parameters4 of [
        (action, s) ->
          {
            const card_pattern : card_pattern
            = case s.card_patterns [action.card_to_buy]  of [
                Some (pattern) -> pattern
              | None (Unit) ->
                  (failwith ("buy_single: No card pattern.")
                   : card_pattern)
              ];

            const price : tez
            = Operator.times
                (card_pattern.coefficient,
                 Operator.add (card_pattern.quantity, 1n));

            if Operator.gt (price, Tezos.get_amount (Unit))
            then failwith ("Not enough money")
            else skip;

            card_pattern :=
              card_pattern.quantity with
                Operator.add (card_pattern.quantity, 1n);

            const card_patterns : card_patterns
            = s.card_patterns;

            card_patterns :=
              Map.add
                (action.card_to_buy,
                 card_pattern,
                 card_patterns);

            s := s.card_patterns with card_patterns;

            const cards : cards = s.cards;

            cards :=
              Map.add
                (s.next_id,
                 record [
                   card_owner = Tezos.get_sender (Unit);
                   card_pattern = action.card_to_buy
                 ],
                 cards);

            s := s.cards with cards;

            s := s.next_id with Operator.add (s.next_id, 1n)
          } with ((list [] : list (operation)), s)
      ]

    function main (const gen__parameters5 : parameter * storage) is
      case gen__parameters5 of [
        (action, s) ->
          case action of [
            Buy_single (bs) -> buy_single (bs, s)
          | Sell_single (as) -> sell_single (as, s)
          | Transfer_single (at) -> transfer_single (at, s)
          ]
      ] |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/coase.ligo" ; "cameligo" ] ;
  [%expect {|
    type card_pattern_id = nat

    type card_pattern = {coefficient : tez; quantity : nat}

    type card_patterns = (card_pattern_id, card_pattern) map

    type card_id = nat

    type card =
      {card_owner : address; card_pattern : card_pattern_id}

    type cards = (card_id, card) map

    type storage =
      {card_patterns : card_patterns;
       cards : cards;
       next_id : nat}

    type return = operation list * storage

    type action_buy_single = {card_to_buy : card_pattern_id}

    type action_sell_single = {card_to_sell : card_id}

    type action_transfer_single =
      {card_to_transfer : card_id; destination : address}

    type parameter =
      Buy_single of action_buy_single
    | Sell_single of action_sell_single
    | Transfer_single of action_transfer_single

    let transfer_single
    : action_transfer_single * storage -> return =
      (fun gen__parameters2 : action_transfer_single * storage ->
         match gen__parameters2 with
         (action, [@var] s) ->
             let [@var] cards : cards = s.cards in
             let [@var] card : card =
               match Map.find_opt action.card_to_transfer cards
               with
                 Some card -> card
               | None ->
                   (failwith "transfer_single: No card." : card) in
             begin
               if (card.card_owner <> Tezos.get_sender ())
               then failwith "This card doesn't belong to you"
               else ();
               begin
                 let [@var] card =
                   {card with
                     {card_owner = action.destination}} in
                 ();
                 begin
                   let [@var] cards =
                     (Map.add
                        (action.card_to_transfer)
                        (card)
                        (cards)) in
                   ();
                   begin
                     let [@var] s = {s with {cards = cards}} in
                     ();
                     ([] : operation list), s
                   end
                 end
               end
             end)

    let sell_single : action_sell_single * storage -> return =
      (fun gen__parameters3 : action_sell_single * storage ->
         match gen__parameters3 with
         (action, [@var] s) ->
             let card : card =
               match Map.find_opt action.card_to_sell s.cards
               with
                 Some card -> card
               | None ->
                   (failwith "sell_single: No card." : card) in
             begin
               if (card.card_owner <> Tezos.get_sender ())
               then failwith "This card doesn't belong to you"
               else ();
               let [@var] card_pattern : card_pattern =
                 match Map.find_opt
                         card.card_pattern
                         s.card_patterns
                 with
                   Some pattern -> pattern
                 | None ->
                     (failwith "sell_single: No card pattern."
                      : card_pattern) in
               begin
                 let [@var] card_pattern =
                   {card_pattern with
                     {quantity =
                        abs (card_pattern.quantity - 1n)}} in
                 ();
                 let [@var] card_patterns : card_patterns =
                   s.card_patterns in
                 begin
                   let [@var] card_patterns =
                     (Map.add
                        (card.card_pattern)
                        (card_pattern)
                        (card_patterns)) in
                   ();
                   begin
                     let [@var] s =
                       {s with
                         {card_patterns = card_patterns}} in
                     ();
                     let [@var] cards : cards = s.cards in
                     begin
                       let [@var] cards =
                         (Map.remove
                            (action.card_to_sell)
                            (cards)) in
                       ();
                       begin
                         let [@var] s = {s with {cards = cards}} in
                         ();
                         let price : tez =
                           (card_pattern.coefficient
                            * card_pattern.quantity) in
                         let receiver : unit contract =
                           match (Tezos.get_contract_opt
                                    (Tezos.get_sender ())
                                  : unit contract option)
                           with
                             Some contract -> contract
                           | None ->
                               (failwith
                                  "sell_single: No contract."
                                : unit contract) in
                         let op : operation =
                           Tezos.transaction unit price receiver in
                         let operations : operation list = [op] in
                         operations, s
                       end
                     end
                   end
                 end
               end
             end)

    let buy_single : action_buy_single * storage -> return =
      (fun gen__parameters4 : action_buy_single * storage ->
         match gen__parameters4 with
         (action, [@var] s) ->
             let [@var] card_pattern : card_pattern =
               match Map.find_opt
                       action.card_to_buy
                       s.card_patterns
               with
                 Some pattern -> pattern
               | None ->
                   (failwith "buy_single: No card pattern."
                    : card_pattern) in
             let price : tez =
               (card_pattern.coefficient
                * (card_pattern.quantity + 1n)) in
             begin
               if (price > Tezos.get_amount ())
               then failwith "Not enough money"
               else ();
               begin
                 let [@var] card_pattern =
                   {card_pattern with
                     {quantity = (card_pattern.quantity + 1n)}} in
                 ();
                 let [@var] card_patterns : card_patterns =
                   s.card_patterns in
                 begin
                   let [@var] card_patterns =
                     (Map.add
                        (action.card_to_buy)
                        (card_pattern)
                        (card_patterns)) in
                   ();
                   begin
                     let [@var] s =
                       {s with
                         {card_patterns = card_patterns}} in
                     ();
                     let [@var] cards : cards = s.cards in
                     begin
                       let [@var] cards =
                         (Map.add
                            (s.next_id)
                            ({card_owner = Tezos.get_sender ();
                              card_pattern = action.card_to_buy})
                            (cards)) in
                       ();
                       begin
                         let [@var] s = {s with {cards = cards}} in
                         ();
                         begin
                           let [@var] s =
                             {s with
                               {next_id = (s.next_id + 1n)}} in
                           ();
                           ([] : operation list), s
                         end
                       end
                     end
                   end
                 end
               end
             end)

    let main : parameter * storage -> return =
      (fun gen__parameters5 : parameter * storage ->
         match gen__parameters5 with
         (action, s) ->
             match action with
               Buy_single bs -> buy_single bs s
             | Sell_single as -> sell_single as s
             | Transfer_single at -> transfer_single at s) |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/coase.ligo" ; "reasonligo" ] ;
  [%expect {|
type card_pattern_id = nat;

type card_pattern = {coefficient: tez, quantity: nat};

type card_patterns = map(card_pattern_id, card_pattern);

type card_id = nat;

type card = {
  card_owner: address,
  card_pattern: card_pattern_id};

type cards = map(card_id, card);

type storage = {card_patterns, cards, next_id: nat};

type return = (list(operation), storage);

type action_buy_single = {card_to_buy: card_pattern_id};

type action_sell_single = {card_to_sell: card_id};

type action_transfer_single = {
  card_to_transfer: card_id,
  destination: address};

type parameter =
  Buy_single(action_buy_single)
| Sell_single(action_sell_single)
| Transfer_single(action_transfer_single);

let transfer_single
: (action_transfer_single, storage) => return =
  ((gen__parameters2: (action_transfer_single, storage))
   : return =>
     switch  gen__parameters2 {
     | (action, [@var] s) =>
         let [@var] cards: cards = s.cards;
         let [@var] card: card =
           switch
           Map.find_opt(action.card_to_transfer, cards) {
           | Some card => card
           | None() =>
               (failwith("transfer_single: No card.") : card)
           };
         {
           if(((card.card_owner) != (Tezos.get_sender(())))) {
             failwith("This card doesn't belong to you")
           } else {

             ()
             };
           {
             let [@var] card =
               {...card,
                 {card_owner: action.destination}};
             ();
             {
               let [@var] cards =
                 (
                  Map.add((action.card_to_transfer),
                     (card),
                     (cards)));
               ();
               {
                 let [@var] s = {...s, {cards: cards}};
                 ();
                 ([] : list(operation)), s
               }
             }
           }
         }
     });

let sell_single: (action_sell_single, storage) => return =
  ((gen__parameters3: (action_sell_single, storage)): return =>
     switch  gen__parameters3 {
     | (action, [@var] s) =>
         let card: card =
           switch Map.find_opt(action.card_to_sell, s.cards) {
           | Some card => card
           | None() =>
               (failwith("sell_single: No card.") : card)
           };
         {
           if(((card.card_owner) != (Tezos.get_sender(())))) {
             failwith("This card doesn't belong to you")
           } else {

             ()
             };
           let [@var] card_pattern: card_pattern =
             switch
             Map.find_opt(card.card_pattern, s.card_patterns) {
             | Some pattern => pattern
             | None() =>
                 (failwith("sell_single: No card pattern.")
                   : card_pattern)
             };
           {
             let [@var] card_pattern =
               {...card_pattern,
                 {
                   quantity:
                     abs(((card_pattern.quantity) - (1n)))}};
             ();
             let [@var] card_patterns: card_patterns =
               s.card_patterns;
             {
               let [@var] card_patterns =
                 (
                  Map.add((card.card_pattern),
                     (card_pattern),
                     (card_patterns)));
               ();
               {
                 let [@var] s =
                   {...s,
                     {card_patterns: card_patterns}};
                 ();
                 let [@var] cards: cards = s.cards;
                 {
                   let [@var] cards =
                     (
                      Map.remove((action.card_to_sell),
                         (cards)));
                   ();
                   {
                     let [@var] s = {...s, {cards: cards}};
                     ();
                     let price: tez =
                       ((card_pattern.coefficient) * (card_pattern.
                           quantity));
                     let receiver: contract(unit) =
                       switch (
                         Tezos.get_contract_opt(
                           Tezos.get_sender(()))
                         : option(contract(unit))) {
                       | Some contract => contract
                       | None() =>
                           (
                             failwith("sell_single: No contract.")
                             : contract(unit))
                       };
                     let op: operation =

                       Tezos.transaction(unit,
                          price,
                          receiver);
                     let operations: list(operation) = [op];
                     operations, s
                   }
                 }
               }
             }
           }
         }
     });

let buy_single: (action_buy_single, storage) => return =
  ((gen__parameters4: (action_buy_single, storage)): return =>
     switch  gen__parameters4 {
     | (action, [@var] s) =>
         let [@var] card_pattern: card_pattern =
           switch
           Map.find_opt(action.card_to_buy, s.card_patterns) {
           | Some pattern => pattern
           | None() =>
               (failwith("buy_single: No card pattern.")
                 : card_pattern)
           };
         let price: tez =
           ((card_pattern.coefficient) * (((card_pattern.
                 quantity) + (1n))));
         {
           if(((price) > (Tezos.get_amount(())))) {
             failwith("Not enough money")
           } else {

             ()
             };
           {
             let [@var] card_pattern =
               {...card_pattern,
                 {
                   quantity:
                     ((card_pattern.quantity) + (1n))}};
             ();
             let [@var] card_patterns: card_patterns =
               s.card_patterns;
             {
               let [@var] card_patterns =
                 (
                  Map.add((action.card_to_buy),
                     (card_pattern),
                     (card_patterns)));
               ();
               {
                 let [@var] s =
                   {...s,
                     {card_patterns: card_patterns}};
                 ();
                 let [@var] cards: cards = s.cards;
                 {
                   let [@var] cards =
                     (
                      Map.add((s.next_id),
                         ({
                           card_owner: Tezos.get_sender(()),
                           card_pattern: action.card_to_buy}),
                         (cards)));
                   ();
                   {
                     let [@var] s = {...s, {cards: cards}};
                     ();
                     {
                       let [@var] s =
                         {...s,
                           {next_id: ((s.next_id) + (1n))}};
                       ();
                       ([] : list(operation)), s
                     }
                   }
                 }
               }
             }
           }
         }
     });

let main: (parameter, storage) => return =
  ((gen__parameters5: (parameter, storage)): return =>
     switch  gen__parameters5 {
     | (action, s) =>
         switch  action {
         | Buy_single bs => buy_single(bs, s)
         | Sell_single as => sell_single(as, s)
         | Transfer_single at => transfer_single(at, s)
         }
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/deep_access.ligo" ; "pascaligo" ] ;
  [%expect{|
    type pii is int * int

    type ppi is record [x : pii; y : pii]

    type ppp is ppi * ppi

    function main (const gen___2 : unit) is
    {
      const a : ppp
      = (record [x = (0, 1); y = (10, 11)],
         record [x = (100, 101); y = (110, 111)]);

      a := a.0 with a.0.x with a.0. x.0 with 2
    } with a.0. x. 0

    function asymetric_tuple_access (const gen___3 : unit) is
    {
      const tuple : int * int * int * int = (0, (1, (2, 3)))
    } with
        Operator.add
          (Operator.add
             (Operator.add (tuple.0, tuple.1. 0),
              tuple.1. 1. 0),
           tuple.1. 1. 1)

    type nested_record_t is
      record [nesty : record [mymap : map (int, string)]]

    function nested_record (const nee : nested_record_t) is
    {
      nee :=
        nee.nesty with
          nee.nesty.mymap with
            Map.add (1, "one", nee.nesty. mymap)
    } with
        case nee.nesty. mymap [1]  of [
          Some (s) -> s
        | None (Unit) ->
            (failwith ("Should not happen.") : string)
        ] |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/deep_access.ligo" ; "cameligo" ] ;
  [%expect{|
    type pii = int * int

    type ppi = {x : pii; y : pii}

    type ppp = ppi * ppi

    let main : unit -> int =
      (fun gen___2 : unit ->
         let [@var] a : ppp =
           {x = 0, 1; y = 10, 11}, {x = 100, 101; y = 110, 111} in
         begin
           let [@var] a =
             {a with
               {0 = {a with {0.x = {a with {0.x.0 = 2}}}}}} in
           ();
           a.0.x.0
         end)

    let asymetric_tuple_access : unit -> int =
      (fun gen___3 : unit ->
         let [@var] tuple : int * int * int * int = 0, 1, 2, 3 in
         (((tuple.0 + tuple.1.0) + tuple.1.1.0) + tuple.1.1.1))

    type nested_record_t = {nesty : {mymap : (int, string) map}}

    let nested_record : nested_record_t -> string =
      (fun [@var] nee : nested_record_t ->
         begin
           let [@var] nee =
             {nee with
               {nesty =
                  {nee with
                    {nesty.mymap =
                       (Map.add (1) ("one") (nee.nesty.mymap))}}}} in
           ();
           match Map.find_opt 1 nee.nesty.mymap with
             Some s -> s
           | None -> (failwith "Should not happen." : string)
         end) |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/deep_access.ligo" ; "reasonligo" ] ;
  [%expect{|
type pii = (int, int);

type ppi = {x: pii, y: pii};

type ppp = (ppi, ppi);

let main: unit => int =
  ((gen___2: unit): int =>
     let [@var] a: ppp =
       {
          x: 0, 1,
          y: 10, 11}, {x: 100, 101, y: 110, 111};
     {
       let [@var] a =
         {...a,
           {0: {...a, {0.x: {...a, {0.x[0]: 2}}}}}};
       ();
       a[0].x[0]
     });

let asymetric_tuple_access: unit => int =
  ((gen___3: unit): int =>
     let [@var] tuple: (int, (int, (int, int))) = 0, 1, 2, 3;
     ((((((tuple[0]) + (tuple[1][0]))) + (tuple[1][1][0]))) + (tuple[1][1][1])));

type nested_record_t = {nesty: {mymap: map(int, string)}};

let nested_record: nested_record_t => string =
  (([@var] nee: nested_record_t): string => {
     let [@var] nee =
       {...nee,
         {
           nesty:
             {...nee,
               {
                 nesty.mymap:
                   (Map.add((1), ("one"), (nee.nesty.mymap)))}}}};
     ();
     switch Map.find_opt(1, nee.nesty.mymap) {
     | Some s => s
     | None() => (failwith("Should not happen.") : string)
     }
   }); |}]

(*
let%expect_test _ =
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/double_fold_converter.religo" ; "pascaligo" ] ;
  [%expect{|
type tokenId is nat

type tokenOwner is address

type tokenAmount is nat

type transferContents is
  record [
    amount : tokenAmount;
    to_ : tokenOwner;
    token_id : tokenId
  ]

type transfer is
  record [from_ : tokenOwner; txs : list (transferContents)]

type transferContentsMichelson is
  michelson_pair_right_comb (transferContents)

type transferAuxiliary is
  record [
    from_ : tokenOwner;
    txs : list (transferContentsMichelson)
  ]

type transferMichelson is
  michelson_pair_right_comb (transferAuxiliary)

type transferParameter is list (transferMichelson)

type parameter is Transfer of transferParameter

type storage is big_map (tokenId, tokenOwner)

type entrypointParameter is parameter * storage

type entrypointReturn is list (operation) * storage

const errorTokenUndefined = "TOKEN_UNDEFINED"

const errorNotOwner = "NOT_OWNER"

const errorInsufficientBalance = "INSUFFICIENT_BALANCE"

type transferContentsIteratorAccumulator is
  storage * tokenOwner

function transferContentsIterator
  (const gen__1 :
     transferContentsIteratorAccumulator *
     transferContentsMichelson) is
block {
  const accumulator = gen__1.0;
  const transferContentsMichelson = gen__1.1;
  const gen__2 = accumulator;
  const storage = gen__2.0;
  const from_ = gen__2.1;
  const transferContents : transferContents
  = Layout.convert_from_right_comb
      (transferContentsMichelson);
  const tokenOwner : option (tokenOwner)
  = Map.find_opt (transferContents.token_id, storage);
  const tokenOwner
  = case tokenOwner of [
      Some (tokenOwner) ->
        if EQ (tokenOwner, from_)
        then tokenOwner
        else
          (failwith (errorInsufficientBalance) : tokenOwner)
    | None -> (failwith (errorTokenUndefined) : tokenOwner)
    ];
  const storage
  = Map.update
      (transferContents.token_id,
       Some (transferContents.to_), storage)
} with (storage, from_)

function allowOnlyOwnTransfer (const from : tokenOwner) is
  if NEQ (from, Tezos.sender)
  then failwith (errorNotOwner)
  else Unit

function transferIterator
  (const gen__3 : storage * transferMichelson) is
block {
  const storage = gen__3.0;
  const transferMichelson = gen__3.1;
  const transferAuxiliary2 : transferAuxiliary
  = Layout.convert_from_right_comb (transferMichelson);
  const from_ : tokenOwner = transferAuxiliary2.from_;
  allowOnlyOwnTransfer (from_);
  const gen__5
  = List.fold
      (transferContentsIterator, transferAuxiliary2.txs,
       (storage, from_));
  const storage = gen__5.0;
  const gen__4 = gen__5.1
} with storage

function transfer
  (const gen__6 : transferParameter * storage) is
block {
  const transferParameter = gen__6.0;
  const storage = gen__6.1;
  const storage
  = List.fold (transferIterator, transferParameter, storage)
} with ((list [] : list (operation)), storage)

function main (const gen__7 : entrypointParameter) is
block {
  const parameter = gen__7.0;
  const storage = gen__7.1
} with
    case parameter of [
      Transfer (transferParameter) ->
        transfer (transferParameter, storage)
    ] |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/double_fold_converter.religo" ; "cameligo" ] ;
  [%expect.unreachable];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/double_fold_converter.religo" ; "reasonligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at file "src/bin/expect_tests/cli_expect.ml", line 28, characters 7-29
  Called from file "src/bin/expect_tests/transpiler_test.ml", line 779, characters 2-111
  Called from file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts/double_fold_converter.religo", line 31, characters 45-102:
   30 |     let (storage, from_) = accumulator;
   31 |     let transferContents: transferContents = Layout.convert_from_right_comb(transferContentsMichelson);
   32 |     let tokenOwner: option(tokenOwner) = Map.find_opt(transferContents.token_id, storage);

  Unknown constant: Layout.convert_from_right_comb |}]
*)

let%expect_test _ =
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/failwith.ligo" ; "pascaligo" ] ;
  [%expect {|
type parameter is Pos of nat | Zero of nat

type storage is unit

type return is list (operation) * storage

function main (const gen__parameters2 : parameter * storage) is
  case gen__parameters2 of [
    (p, s) ->
      {
        case p of [
          Zero (n) ->
            if Operator.gt (n, 0n)
            then failwith ("fail")
            else skip
        | Pos (n) ->
            if Operator.eq (n, 0n)
            then failwith ("fail")
            else skip
        ]
      } with ((list [] : list (operation)), s)
  ]

function foobar (const i : int) is
{
  const p : parameter = (Zero (42n));

  if Operator.gt (i, 0)
  then {
    i := Operator.add (i, 1);

    if Operator.gt (i, 10)
    then {
      i := 20;

      failwith ("who knows");

      i := 30
    }
    else skip
  }
  else
    case p of [
      Zero (gen___5) -> failwith (42n)
    | Pos (gen___6) -> skip
    ]
} with
    case p of [
      Zero (gen___3) -> i
    | Pos (gen___4) -> (failwith ("waaaa") : int)
    ]

function failer (const p : int) is
{
  if Operator.eq (p, 1) then failwith (42) else skip
} with p |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/failwith.ligo" ; "cameligo" ] ;
  [%expect {|
type parameter = Pos of nat | Zero of nat

type storage = unit

type return = operation list * storage

let main : parameter * storage -> return =
  (fun gen__parameters2 : parameter * storage ->
     match gen__parameters2 with
     (p, s) ->
         begin
           match p with
             Zero n ->
               if (n > 0n) then failwith "fail" else ()
           | Pos n ->
               if (n = 0n) then failwith "fail" else ();
           ([] : operation list), s
         end)

let foobar : int -> int =
  (fun [@var] i : int ->
     let [@var] p : parameter = (Zero 42n) in
     begin
       if (i > 0)
       then
         begin
           let [@var] i = (i + 1) in
           ();
           if (i > 10)
           then
             begin
               let [@var] i = 20 in
               ();
               begin
                 failwith "who knows";
                 let [@var] i = 30 in
                 ()
               end
             end
           else ()
         end
       else
         match p with
           Zero gen___5 -> failwith 42n
         | Pos gen___6 -> ();
       match p with
         Zero gen___3 -> i
       | Pos gen___4 -> (failwith "waaaa" : int)
     end)

let failer : int -> int =
  (fun p : int ->
     begin
       if (p = 1) then failwith 42 else ();
       p
     end) |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/failwith.ligo" ; "reasonligo" ] ;
  [%expect {|
type parameter = Pos(nat) | Zero(nat);

type storage = unit;

type return = (list(operation), storage);

let main: (parameter, storage) => return =
  ((gen__parameters2: (parameter, storage)): return =>
     switch  gen__parameters2 {
     | (p, s) =>
         {
           switch  p {
           | Zero n =>
               if(((n) > (0n))) {
                 failwith("fail")
               } else {

                 ()
                 }
           | Pos n =>
               if(((n) == (0n))) {
                 failwith("fail")
               } else {

                 ()
                 }
           };
           ([] : list(operation)), s
         }
     });

let foobar: int => int =
  (([@var] i: int): int =>
     let [@var] p: parameter = (Zero 42n);
     {
       if(((i) > (0))) {

         {
           let [@var] i = ((i) + (1));
           ();
           if(((i) > (10))) {

             {
               let [@var] i = 20;
               ();
               {
                 failwith("who knows");
                 let [@var] i = 30;
                 ()
               }
             }
           } else {

             ()
             }
         }
       } else {

         switch  p {
         | Zero gen___5 => failwith(42n)
         | Pos gen___6 => ()
         }
         };
       switch  p {
       | Zero gen___3 => i
       | Pos gen___4 => (failwith("waaaa") : int)
       }
     });

let failer: int => int =
  ((p: int): int => {
     if(((p) == (1))) {
       failwith(42)
     } else {

       ()
       };
     p
   }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/recursion.ligo" ; "pascaligo" ] ;
  [%expect {|
    recursive function sum (const gen__parameters2 : int * int) is
      case gen__parameters2 of [
        (n, acc) ->
          if Operator.lt (n, 1)
          then acc
          else sum (Operator.sub (n, 1), Operator.add (acc, n))
      ]

    recursive function fibo
      (const gen__parameters3 : int * int * int) is
      case gen__parameters3 of [
        (n, n_1, n_0) ->
          if Operator.lt (n, 2)
          then n_1
          else
            fibo
              (Operator.sub (n, 1),
               Operator.add (n_1, n_0),
               n_1)
      ] |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/recursion.ligo" ; "cameligo" ] ;
  [%expect {|
    let rec sum : int * int -> int =
      (fun gen__parameters2 : int * int ->
         match gen__parameters2 with
         (n, acc) ->
             if (n < 1) then acc else sum (n - 1) (acc + n))

    let rec fibo : int * int * int -> int =
      (fun gen__parameters3 : int * int * int ->
         match gen__parameters3 with
         (n, n_1, n_0) ->
             if (n < 2)
             then n_1
             else fibo (n - 1) (n_1 + n_0) n_1) |}];
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/recursion.ligo" ; "reasonligo" ] ;
  [%expect {|
    let rec sum: (int, int) => int =
      ((gen__parameters2: (int, int)): int =>
         switch  gen__parameters2 {
         | (n, acc) =>
             if(((n) < (1))) {
               acc
             } else {

               sum(((n) - (1)), ((acc) + (n)))
               }
         });

    let rec fibo: (int, int, int) => int =
      ((gen__parameters3: (int, int, int)): int =>
         switch  gen__parameters3 {
         | (n, n_1, n_0) =>
             if(((n) < (2))) {
               n_1
             } else {

               fibo(((n) - (1)), ((n_1) + (n_0)), n_1)
               }
         }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile" ; "contract" ; "../../test/contracts/transpiler_nested.ligo" ; "cameligo" ] ;
  [%expect {|
    let f : nat -> nat = (fun x : nat -> x)

    let bar : nat -> nat = (fun x : nat -> f (f x))
  |}]
