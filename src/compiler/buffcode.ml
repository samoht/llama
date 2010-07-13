(* To buffer bytecode during emission *)

let out_buffer = ref (String.make 64 '\000')
and out_position = ref 0
;;

let realloc_out_buffer () =
  let new_buffer = String.make (2 * String.length !out_buffer) '\000' in
    String.blit !out_buffer 0 new_buffer 0 (String.length !out_buffer);
    out_buffer := new_buffer;
    ()
;;

let init_out_code () =
  out_position := 0;
  ()
;;

let out b =
  if !out_position >= String.length !out_buffer then realloc_out_buffer();
  !out_buffer.[!out_position] <- char_of_int (b land 0xFF);
  incr out_position
;;

let out_short s =
  if s >= 32768 || s < -32768 then
    Error.displacement_overflow ()
  else begin
    out s; out (s lsr 8)
  end
;;

