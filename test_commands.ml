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
let lst = 
  ("super/sub/hello.txt", "hello world")::[]
let () = Commands.commit ("fifth message") ("master") lst