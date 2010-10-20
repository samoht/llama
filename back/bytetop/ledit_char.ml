type encoding = Ascii | Iso_8859 | Utf_8
let encoding = ref Iso_8859

let nbc c =
  if Char.code c < 0b10000000 then 1
  else if Char.code c < 0b11000000 then -1
  else if Char.code c < 0b11100000 then 2
  else if Char.code c < 0b11110000 then 3
  else if Char.code c < 0b11111000 then 4
  else if Char.code c < 0b11111100 then 5
  else if Char.code c < 0b11111110 then 6
  else -1

type t = string

let of_ascii c = String.make 1 c
let to_ascii c =
  if String.length c = 1 && Char.code c.[0] < 128 then Some c.[0]
  else None
let is_word_char c =
  if String.length c = 1 then
    match c.[0] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
      | x ->
          if Char.code x < 128 then false
          else
            match !encoding with
                Ascii -> false
              | Iso_8859 -> Char.code x >= 160
              | Utf_8 -> assert false
  else true
let ctrl_val c =
  if String.length c = 1 then
    let c = c.[0] in
    if Char.code c < 32 || Char.code c == 127 then
      Some (String.make 1 (Char.chr (127 land (Char.code c + 64))))
    else None
  else None
let meta_ctrl_val c =
  if String.length c = 1 then
    let c = c.[0] in
    if Char.code c >= 128 && Char.code c < 160 then
      Some (String.make 1 (Char.chr (127 land (Char.code c + 64))))
    else None
  else None
let not_ascii_val c =
  if String.length c = 1 then
    let c = c.[0] in
    if Char.code c >= 128 then
      let c1 = Char.chr (Char.code c / 100 + Char.code '0') in
      let c2 = Char.chr (Char.code c mod 100 / 10 + Char.code '0') in
      let c3 = Char.chr (Char.code c mod 10 + Char.code '0') in
      Some (String.make 1 c1, String.make 1 c2, String.make 1 c3)
    else None
  else None
let uppercase c =
  match !encoding with
      Ascii | Iso_8859 -> String.uppercase c
    | Utf_8 ->
        if String.length c = 1 then
          if Char.code c.[0] < 128 then String.uppercase c else c
        else c
let lowercase c =
  match !encoding with
      Ascii | Iso_8859 -> String.lowercase c
    | Utf_8 ->
        if String.length c = 1 then
          if Char.code c.[0] < 128 then String.lowercase c else c
        else c
let to_string s = s
let get_char f =
  match !encoding with
      Ascii | Iso_8859 -> String.make 1 (f ())
    | Utf_8 ->
        let c = f () in
        let nbc = nbc c in
        if nbc < 0 then "?"
        else if nbc = 1 then String.make 1 c
        else
          let rec loop s n =
            if n = 0 then s
            else
              let c = f () in
              if Char.code c < 0b10000000 then "?"
              else if Char.code c > 0b10111111 then "?"
              else loop (s ^ String.make 1 c) (n - 1)
          in
          loop (String.make 1 c) (nbc - 1)
let input ic = get_char (fun () -> input_char ic)
let read =
  let buff = " " in
  fun () ->
    get_char
      (fun () ->
         let len = Unix.read Unix.stdin buff 0 1 in
         if len == 0 then raise End_of_file else buff.[0])
let parse s = get_char (fun () -> Stream.next s)
let print c =
  if String.length c = 1 && c.[0] = '\n' then print_newline ()
  else print_string c
let prerr c = output_string stderr c
let prerr_backsp c =
  if !encoding = Utf_8 && Char.code c.[0] >= 228 &&
    Char.code c.[0] <= 233
  then
    (* hack for Chinese; it seems that terminals (at least
       "konsole" and "xterm") need 2 backspaces to put the
       cursor on the glyph. *)
    output_char stderr '\b';
  output_char stderr '\b'
