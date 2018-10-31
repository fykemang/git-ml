open Util
open GitTree
open Commands 

let lst = 
  ("dir/dir/test2.html", "test2")::
  ("dir/dir/test3.html", "<body>test3</body>")::
  ("test1.html","test1")::
  ("dir1/dir2/test.html", "<b>right order</b>")::
  ("dir/test2.html","test1")::
  ("superdirectory/subdirectory/file.txt", "10/24/2018")::
  ("dummy.txt", "some text")::[]
let lst_ = 
  ("super/sub/hello.txt", "hello world")::[]
let a b = Commands.commit ("fifth message") ("master") lst
let () = 
  (Commands.file_list_to_tree lst) |> GitTree.pp_git_tree "" |> 
  print_endline
let () = print_endline "Printing Tree from File Structure:"
let () = (Commands.current_head_to_git_tree ()) |> GitTree.pp_git_tree "" |> 
         print_endline