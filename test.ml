open OUnit2
open Parse
open GitTree

let b1 = GitTree.Node (Blob "b1", [])
let b2 = GitTree.Node (Blob "b2", [])
let b3 = GitTree.Node (Blob "b3", [])
let t1 = GitTree.Node (File "f1", [b1])
let t2 = GitTree.Node (File "f2", [b2])
let t3 = GitTree.Node (File "f3", [b3])
let treetmp = Node (Tree_Object ".", [t1; t2])
let tree = add_child_tree t3 treetmp

let make_size_test
    (name : string)
    (tree: GitTree.t)
    (expected_output : int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (size tree) ~printer: string_of_int)

let make_value_test
    (name: string)
    (tree: GitTree.t)
    (expected_output: git_object) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (value tree))

let tree_tests = [
  (* testing size of tree *)
  make_size_test "b1size" b1 1;
  make_size_test "b2size" b2 1;
  make_size_test "b3size" b3 1;
  make_size_test "t1size" t1 2;
  make_size_test "t2size" t2 2;
  make_size_test "t3size" t3 2;
  make_size_test "treetmpsize" treetmp 5;
  make_size_test "treesize" tree 7;

  (* testing value of tree *)
  make_value_test "b1val" b1 (Blob "b1");
  make_value_test "b2val" b2 (Blob "b2");
  make_value_test "b3val" b3 (Blob "b3");
  make_value_test "t1val" t1 (File "f1");
  make_value_test "t2val" t2 (File "f2");
  make_value_test "t3val" t3 (File "f3");
  make_value_test "treeval" tree (Tree_Object ".");

]


let parse_tests = [

]
let suite = "TEST SUITE" >::: List.flatten [tree_tests]
let _ = run_test_tt_main suite