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

#import "util.mligo" "Util"
#import "logger.mligo" "Logger"
#import "result.mligo" "Result"
#import "ansi.mligo" "Ansi"

(** Describes the model of the test engine. *)

type case = {
  case_name : string
; case_desc : string
; case_task : (Logger.level -> Result.result)
}

type suite = {
  suite_name : string
; suite_cases : case list
}

(** [case name desc test] will produce a test. *)
let case
  (name: string)
  (desc: string)
  (task: Logger.level -> Result.result) : case
= { case_name = name
  ; case_desc = desc
  ; case_task = task
  }


(** [suite name cases] will produce a test suite. *)
let suite (name: string) (cases : case list) : suite = {
  suite_name = name
; suite_cases = cases
}

(** Compute a test case and return the status and the computed message. *)
let perform_case (log_level: Logger.level) (case : case) : (bool * string) =
  let result = case.case_task log_level in
  let is_succeed = Result.is_succeed result in
  let message = Result.pp result in
  (is_succeed, message)

(** Pretty print the result of a test. *)
let pp_case_result (is_succeed : bool) (message : string) (case : case) =
  let flag, color = if is_succeed then "v", Ansi.green else "x", Ansi.red in
  let full_message =
    "[" ^ (color flag) ^ "]"
    ^ case.case_name ^ " | "
    ^ case.case_desc ^ ":\n    "
    ^ color message
  in
  Test.println full_message

(** Just perform a test case and returns is succeed status. *)
let run_case (log_level: Logger.level) (case : case) : bool =
  let (is_succeed, message) = perform_case log_level case in
  let () = pp_case_result is_succeed  message case in
  is_succeed

let line = "-----------------------------------"

(** Run a test suite and pretty print the result. *)
let run_suite (log_level: Logger.level) (suite: suite) : bool =
  let () = Test.println line in
  let () = Test.println ("Running " ^ "<" ^ suite.suite_name ^ ">") in
  let () = Test.println line in
  let failed_test =
    List.fold_left (fun (acc, case : nat * case) ->
      let is_succeed = run_case log_level case in
      if is_succeed then acc else acc + 1n
    ) 0n suite.suite_cases
 in
 let () = Test.println line in
 let is_succeed = failed_test = 0n in
 let () =
   if not is_succeed then
     let message =
        "There are some tests ("
        ^ Util.nat_to_string_without_suffix failed_test
        ^ ") that fail"
     in Test.println (Ansi.red message)

 in
 is_succeed

(** Run a list of test suites. *)
let run_suites (log_level : Logger.level) (suites: suite list) =
  let failed_suites =
    List.fold_left (fun (acc, suite : nat * suite) ->
      let is_succeed = run_suite log_level suite in
      if is_succeed then acc else acc + 1n
    ) 0n suites
  in
  let is_succeed = failed_suites = 0n in
  let () = Test.println ("\n\n" ^ line) in
  if not is_succeed then
    Test.failwith
       ("There are some suites ("
        ^ Util.nat_to_string_without_suffix failed_suites
        ^ ") that fail")
