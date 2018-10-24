open OUnit2
open Parse



let parse_tests = [

]
let suite = "TEST SUITE" >::: List.flatten []
let _ = run_test_tt_main suite