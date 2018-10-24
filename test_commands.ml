open Util
open GitTree
open Commands 

let lst = 
  ("test1.html","<body>test1</body")::("dir/test2.html","<body>test2</body")::[]
let () = Commands.commit ("commit message") ("master") lst 