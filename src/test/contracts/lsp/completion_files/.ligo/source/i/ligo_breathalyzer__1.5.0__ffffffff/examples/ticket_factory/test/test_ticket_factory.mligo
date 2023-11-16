(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)


#import "../src/mint_sc.mligo" "Mint"
#import "../src/oven_sc.mligo" "Oven"
#import "../../../lib/lib.mligo" "Breath"
#import "util.mligo" "Util"

let case_happy_path =
  Breath.Model.case
    "request_mint and request_redeem"
    "An happy path when everybody send correct values to the mint contract"
    (fun (level: Breath.Logger.level) ->
      let (operator, (alice, bob, carol)) = Breath.Context.init_default () in

      let () = Breath.Logger.log level "Initialize Mint" in

      let mint = Breath.Context.act_as operator
        (Util.originate_mint level 0x01 1tez)
      in

      let () = Breath.Logger.log level "Initialize Oven" in

      let alice_oven = Breath.Context.act_as alice
        (Util.originate_oven level alice mint)
      in

      let bob_oven = Breath.Context.act_as bob
        (Util.originate_oven level bob mint)
       in

      let carol_oven = Breath.Context.act_as carol
        (Util.originate_oven level carol mint)
      in

      let () = Breath.Logger.log level "Perform some mint" in

      let alice_action_1 = Breath.Context.act_as alice
        (Util.request_mint alice_oven 1tez)
      in

      let bob_action_1 = Breath.Context.act_as bob
        (Util.request_mint bob_oven 2tez)
      in

      let carol_action_1 =  Breath.Context.act_as carol
        (Util.request_mint carol_oven 3tez)
      in

      (* We store the result of the current state in order to be able to test
         everything at the end. *)

      let mint_step = Breath.Result.reduce [
          alice_action_1
        ; bob_action_1
        ; carol_action_1
        ; Util.expected_mint_state mint 0x01 1tez 6tez
        ; Util.expected_oven_state alice_oven alice mint 1000000n
        ; Util.expected_oven_state bob_oven bob mint 2000000n
        ; Util.expected_oven_state carol_oven carol mint 3000000n ]
      in

      let () = Breath.Logger.log level "Perform some redeem" in

      let alice_action_2 = Breath.Context.act_as alice
        (Util.request_redeem alice_oven)
      in

      let carol_action_2 = Breath.Context.act_as carol
        (Util.request_redeem carol_oven)
      in

      let redeem_step = Breath.Result.reduce [
          alice_action_2
        ; carol_action_2
        ; Util.expected_mint_state mint 0x01 1tez 2tez
        ; Util.expected_oven_state alice_oven alice mint 0n
        ; Util.expected_oven_state bob_oven bob mint 2000000n
        ; Util.expected_oven_state carol_oven carol mint 0n ]
      in

      Breath.Result.reduce [ mint_step; redeem_step ]
    )

let case_bob_try_to_steal_alice_ticket_bouuuh =
  Breath.Model.case
    "request_mint and request_redeem"
    "Alice mint some ticket and that betrayer Bob is trying to get them back"
    (fun (level: Breath.Logger.level) ->
      let (operator, (alice, bob, _)) = Breath.Context.init_default () in

      let () = Breath.Logger.log level "Initialize Mint" in

      let mint = Breath.Context.act_as operator
        (Util.originate_mint level 0x01 1tez)
      in

      let () = Breath.Logger.log level "Initialize Oven" in

      let alice_oven = Breath.Context.act_as alice
        (Util.originate_oven level alice mint)
      in

      let () = Breath.Logger.log level "Perform some mint" in

      let alice_action_1 = Breath.Context.act_as alice
        (Util.request_mint alice_oven 1tez)
      in

      (* We store the result of the current state in order to be able to test
         everything at the end. *)

      let mint_step = Breath.Result.reduce [
          alice_action_1
        ; Util.expected_mint_state mint 0x01 1tez 1tez
        ; Util.expected_oven_state alice_oven alice mint 1000000n ]
      in

      let () =
        Breath.Logger.log level
          "Perform some redeem, this now that Bon takes the decision to become the bad guy"
      in

      let bob_betrayal = Breath.Context.act_as bob
        (Util.request_redeem alice_oven) (* Bob try to steal alice money *)
      in

      Breath.Result.reduce [
        mint_step
        (* But that was without counting ... the security of our contract! *)
      ; Breath.Expect.fail_with_message "oven_sc: not owner" bob_betrayal
        (* Nothing is changed! *)
      ; Util.expected_mint_state mint 0x01 1tez 1tez
      ; Util.expected_oven_state alice_oven alice mint 1000000n ]
    )



let () =
  Breath.Model.run_suites Void [
    Breath.Model.suite "Suite for [Ticket factory]" [
      case_happy_path
    ; case_bob_try_to_steal_alice_ticket_bouuuh
    ]
  ]
