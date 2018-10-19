open OUnit2

let suite = "TEST SUITE" >::: List.flatten []
let _ = run_test_tt_main suite