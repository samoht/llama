let rec tailcall4 a b c d =
  if a < 0
  then b
  else tailcall4 (a-1) (b+1) (c+2) (d+3)

let rec tailcall8 a b c d e f g h =
  if a < 0
  then b
  else tailcall8 (a-1) (b+1) (c+2) (d+3) (e+4) (f+5) (g+6) (h+7)

let rec tailcall16 a b c d e f g h i j k l m n o p =
  if a < 0
  then b
  else tailcall16 (a-1) (b+1) (c+2) (d+3) (e+4) (f+5) (g+6) (h+7)
                  (i+8) (j+9) (k+10) (l+11) (m+12) (n+13) (o+14) (p+15)

let indtailcall8 fn a b c d e f g h =
  fn a b c d e f g h

let indtailcall16 fn a b c d e f g h i j k l m n o p =
  fn a b c d e f g h i j k l m n o p 

let _ =
  let n = (* 10000000 *) 1000000 in
  print_int (tailcall4 n 0 0 0); print_newline();
  print_int (tailcall8 n 0 0 0 0 0 0 0); print_newline();
  print_int (tailcall16 n 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0); print_newline();
  print_int (indtailcall8 tailcall8 10 0 0 0 0 0 0 0); print_newline();
  print_int (indtailcall16 tailcall16 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0); print_newline()
