(* The Fibonacci function, once more. *)

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)
;;

if !Sys.interactive then () else
if Array.length Sys.argv <> 2 then begin
  print_string "Usage: fib <number>";
  print_newline()
end else begin
  try
    print_int(fib(int_of_string Sys.argv.(1)));
    print_newline()
  with Failure "int_of_string" ->
    print_string "Bad integer constant";
    print_newline()
end
;;
