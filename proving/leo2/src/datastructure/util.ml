let debuglevel = ref 0
let tmpfiles = ref []

let previous_output_supressed = ref true
let supressed_output_count = ref 0

let sysout n s =
  (* print_string ("debug: "^(string_of_int !debuglevel)^", out: "^(string_of_int n)^"\n"); *)
  if n <= !debuglevel
  then
    (if !previous_output_supressed then print_string "\n";
     print_string s;
     flush stdout;
     previous_output_supressed := false)
  else
    (if !supressed_output_count = 100
     then
      (print_string ".";
       flush stdout;
       supressed_output_count := 0;
       previous_output_supressed := true)
     else
       supressed_output_count := !supressed_output_count +1)


let add_list ht k l =
        Hashtbl.add ht l

let add_elem ht k e =
        if Hashtbl.mem ht k then
          let l = Hashtbl.find ht k in
          if not (List.mem e l) then
            Hashtbl.replace ht k (e::l)
          else ()
        else Hashtbl.add ht k [e]

let remove_list ht k =
        Hashtbl.remove ht k

let remove_elem ht k e =
        if Hashtbl.mem ht k then
        let l = Hashtbl.find ht k in
        if List.mem e l then
        Hashtbl.replace ht k (List.filter (fun e' -> e != e') l)

let remove_elem2 ht k e =
        if Hashtbl.mem ht k then
        let l = Hashtbl.find ht k in
        if List.mem e l then
        let l' = List.filter (fun e' -> e != e') l in
        if l'=[] then Hashtbl.remove ht k
        else Hashtbl.replace ht k l'

let remove_element3 htbl a b =
  let l = ref [] in
  while Hashtbl.mem htbl a do
    l := (Hashtbl.find htbl a)::(!l);
    Hashtbl.remove htbl a
  done;
  List.iter (fun x -> if x != b then Hashtbl.add htbl a x) !l

let replace_element htbl a b c =
  remove_element3 htbl a b;
  Hashtbl.add htbl a c

let concat_unique l1 l2 =
  l1 @ (List.filter (fun x -> not (List.mem x l1)) l2)

let iteri f n =
  let rec iteri i =
    if i=n then []
    else (f i)::(iteri (i+1))
  in iteri 0

let id x = x

let implode glue pieces =
  List.fold_left (fun acc el -> acc^(if acc="" then "" else glue)^el) "" pieces


let clean_symbol s =
  let char_map c =
    let cc = Char.code c in
    if (cc >= (Char.code 'A') && cc <= (Char.code 'Z')) ||
       (cc >= (Char.code 'a') && cc <= (Char.code 'z')) ||
       (cc >= (Char.code '0') && cc <= (Char.code '9'))
    then String.make 1 c
    else 
      match c with
        '.' -> "_"
      | '_' -> "_"
      | '$' -> "s_"
      | '@' -> "at_"
      | '#' -> "x_"
      | _ -> ("_"^(string_of_int (Char.code c))^"_")
  in
  let accu = ref "" in 
  for i = 0 to (String.length s) -1 do
    accu := !accu^(char_map s.[i])
  done;
  !accu

(** migrated here from /src/extensions/pa_timed_enabled.ml : *)

let time_ht = Hashtbl.create 12
  
let start_timer (s:string) =
  try (
    let (_,total) = Hashtbl.find time_ht s in
      Hashtbl.replace time_ht s (Sys.time(),total)
  ) with Not_found -> Hashtbl.add time_ht s (Sys.time(),0.)
    
let stop_timer (s:string) =
  let (run,total) = Hashtbl.find time_ht s in
  let diff = Sys.time() -. run in
    Hashtbl.replace time_ht s (diff,total +. diff)
      
let get_total (s:string) =
  snd (Hashtbl.find time_ht s)
    
let get_all_totals () =
  let times = Hashtbl.fold (fun proc (_,time) acc -> (time,proc)::acc) time_ht [] in
    List.sort (fun (time1,_) (time2,_) -> compare time2 time1) times
      
