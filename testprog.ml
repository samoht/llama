let rec fib n = if n < 2 then 1 else fib(n-1)+fib(n-2);;
print_endline (string_of_int (fib 20));;
flush stdout;;
