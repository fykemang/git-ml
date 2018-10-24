open Util
open GitTree
open Commands 

let lst = 
  ("dir/dir/test2.html", "test2")::
  ("dir/dir/test3.html", "<body>test3</body>")::
  ("test1.html","test1")::
  ("dir1/dir2/test.html", "<b>right order</b>")::
  ("dir/test2.html","test1")::[]
let () = Commands.commit ("commit message") ("master") lst 