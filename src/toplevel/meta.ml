let global_data = ref [||]
let length_global_data () = Array.length !global_data
let get_global_data i = !global_data.(i)
let set_global_data i o = !global_data.(i) <- o
let realloc_global_data n =
  if Array.length !global_data < n then
    let new_ar = Array.make n (Obj.repr 0) in
    Array.blit !global_data 0 new_ar 0 (Array.length !global_data);
    global_data := new_ar
let static_alloc n = String.make n '\000'
let static_free s = ()
let static_resize s n =
  if String.length s < n then
    let new_s = String.make n '\000' in
    String.blit s 0 new_s 0 (String.length s);
    new_s
  else
    s
let interprete code entrypoint len =
  for i=entrypoint to entrypoint+len-1 do
    Printf.printf "%d " (int_of_char code.[i])
  done;
  print_newline ();
  print_endline "not sure what to do, guess i'll return an integer-obj";
  Obj.repr 17
let available_primitives _ = [||]
