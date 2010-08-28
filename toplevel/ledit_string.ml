type t = string array
let empty = [| |]
let of_char c = [| c |]
let of_ascii s =
  Array.init (String.length s)
    (fun i ->
       if Char.code s.[i] < 128 then String.make 1 s.[i]
       else invalid_arg "Ledit_string.of_ascii")
let length = Array.length
let set = Array.set
let get = Array.get
let sub = Array.sub
let concat = Array.append
let input_line ic =
  let s = input_line ic in
  match !Ledit_char.encoding with
      Ledit_char.Ascii | Ledit_char.Iso_8859 -> of_ascii s
    | Ledit_char.Utf_8 ->
        let rec loop list i =
          if i >= String.length s then Array.of_list (List.rev list)
          else
            let n = Ledit_char.nbc s.[i] in
            if n < 0 then loop ("?" :: list) (i + 1)
            else loop (String.sub s i n :: list) (i + n)
        in
        loop [] 0
let output oc s = Array.iter (output_string oc) s
