open OUnit2

open OUnit2

module String = struct
  include String
  let format fmt x = Format.fprintf fmt "%s" x
end
module Diff_eng = Diff_engine.Make(String)
open Diff_eng

let make_diff_test
    (name : string)
    (lst : obj list)
    (lst' : obj list)
    (result : t) =
  name >:: (
    fun _ -> assert_equal result (diff lst lst')
  )

let test1 = ["The"; "quick"; "brown"; "fox"; "jumps"; "over"; "the"; 
             "lazy"; "dog"]
let test1' = ["The"; "slow"; "blue"; "cheese"; "drips"; "over"; "the"; 
              "lazy"; "carrot"]
let test1_res = [Eq ["The"];
                 Del ["quick"; "brown"; "fox"; "jumps"];
                 Add ["slow"; "blue"; "cheese"; "drips"];
                 Eq ["over"; "the"; "lazy"];
                 Del ["dog"];
                 Add ["carrot"]]

let diff_tests = 
  "Test suite for Diff_engine" >::: [
    make_diff_test "Empty Test" [] [] [];
    make_diff_test "Empty Test" test1 test1' test1_res;
  ]

let _ = run_test_tt_main diff_tests