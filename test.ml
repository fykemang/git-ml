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
let pre_blob_1 = "Blob One"
let post_blob_1 = "One"
let pre_blob_2 = "Blob One Two"
let post_blob_2 = "One Two"
let pre_blob_3 = "Blob One Two \n One Two"
let post_blob_3 = "One Two \n One Two"
let pre_blob_4 = "Blob "
let post_blob_4 = ""

let make_remove_blob_test
    (name : string)
    (input: string)
    (expected_output : string) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Util.remove_object_tag "Blob" input) 
        ~printer: (fun x -> x))

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

let remove_blob_tests = [
  make_remove_blob_test "standard" pre_blob_1 post_blob_1;
  make_remove_blob_test "two words" pre_blob_2 post_blob_2;
  make_remove_blob_test "newlines" pre_blob_3 post_blob_3;
  make_remove_blob_test "just header" pre_blob_4 post_blob_4;
]

let suite = "TEST SUITE" >::: List.flatten [
    tree_tests;
    remove_blob_tests
  ]

let _ = run_test_tt_main suite