open List
open Obj

type ('a, 'b, 'c) format = string;;

let fprintf outchan format =
  let rec doprn i =
    if i >= String.length format then
      magic ()
    else
      match String.unsafe_get format i with
        '%' ->
          let j = skip_args (succ i) in
          begin match String.unsafe_get format j with
            '%' ->
              output_char outchan '%';
              doprn (succ j)
          | 's' ->
              magic(fun s ->
                if j <= ((+):int->int->int) i 1 then
                  output_string outchan s
                else begin
                  let p =
                    try
                      int_of_string (String.sub format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg "fprintf: bad %s format" in
                  if p > 0 && String.length s < p then begin
                    output_string outchan
                                  (String.make (p - String.length s) ' ');
                    output_string outchan s
                  end else if p < 0 && String.length s < -p then begin
                    output_string outchan s;
                    output_string outchan
                                  (String.make (-p - String.length s) ' ')
                  end else
                    output_string outchan s
                end;
                doprn (succ j))
          | 'c' ->
              magic(fun c ->
                output_char outchan c;
                doprn (succ j))
          | 'd' | 'o' | 'x' | 'X' | 'u' ->
              magic(doint i j)
          | 'f' | 'e' | 'E' | 'g' | 'G' ->
              magic(dofloat i j)
          | 'b' ->
              magic(fun b ->
                output_string outchan (string_of_bool b);
                doprn (succ j))
          | 'a' ->
              magic(fun printer arg ->
                printer outchan arg;
                doprn(succ j))
          | 't' ->
              magic(fun printer ->
                printer outchan;
                doprn(succ j))
          | c ->
              invalid_arg ("fprintf: unknown format")
          end
      |  c  -> output_char outchan c; doprn (succ i)

  and skip_args j =
    match String.unsafe_get format j with
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
      ' ' | '.' | '-' ->
        skip_args (succ j)
    | c ->
        j
    
  and doint i j n =
    let len = j-i in
    let fmt = String.create (len+2) in
    String.blit format i fmt 0 len;
    String.unsafe_set fmt len 'l';
    String.unsafe_set fmt (len+1) (String.unsafe_get format j);
    output_string outchan (format_int fmt n);
    doprn (succ j)

  and dofloat i j f =
    output_string outchan (format_float (String.sub format i (j-i+1)) f);
    doprn (succ j)

  in doprn 0
;;

let printf fmt = fprintf std_out fmt    (* Don't eta-reduce: this confuses *)
and eprintf fmt = fprintf std_err fmt   (* the intelligent linker *)
;;

let fprint chan str = output_string chan str
and print str = print_string str
and eprint str = prerr_string str
;;

let sprintf format =
  let format = (magic format : string) in
  let res = ref [] in

  let rec doprn start i =
    if i >= String.length format then begin
      if i > start then res := String.sub format start (i-start) :: !res;
      magic(String.concat "" (rev !res))
    end else
      if String.unsafe_get format i != '%' then
        doprn start (i+1)
      else begin
        if i > start then res := String.sub format start (i-start) :: !res;
        let j = skip_args (succ i) in
        match String.unsafe_get format j with
          '%' ->
            doprn j (succ j)
        | 's' ->
            magic(dostring i j)
        | 'c' ->
            magic(fun c ->
                res := String.make 1 c :: !res;
                doprn (succ j) (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            magic(doint i j)
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            magic(dofloat i j)
        | 'b' ->
            magic(fun b ->
                res := string_of_bool b :: !res;
                doprn (succ j) (succ j))
        | 'a' ->
            magic(fun printer arg ->
              res := printer () arg :: !res;
              doprn (succ j) (succ j))
        | 't' ->
            magic(fun printer ->
              res := printer () :: !res;
              doprn (succ j) (succ j))
        | c ->
            invalid_arg ("sprintf: unknown format")
      end

  and skip_args j =
    match String.unsafe_get format j with
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
      ' ' | '.' | '-' ->
        skip_args (succ j)
    | c ->
        j
    
  and dostring i j s =
    if j <= i+1 then
      res := s :: !res
    else begin
      let p =
        try
          int_of_string (String.sub format (i+1) (j-i-1))
        with _ ->
          invalid_arg "sprintf: bad %s format" in
      if p > 0 && String.length s < p then begin
        res := String.make (p - String.length s) ' ' :: !res;
        res := s :: !res
      end else if p < 0 && String.length s < -p then begin
        res := s :: !res;
        res := String.make (-p - String.length s) ' ' :: !res
      end else
        res := s :: !res
    end;
    doprn (succ j) (succ j)

  and doint i j n =
    let len = j-i in
    let fmt = String.create (len+2) in
    String.blit format i fmt 0 len;
    String.unsafe_set fmt len 'l';
    String.unsafe_set fmt (len+1) (String.unsafe_get format j);
    res := format_int fmt n :: !res;
    doprn (succ j) (succ j)

  and dofloat i j f =
    res := format_float (String.sub format i (j-i+1)) f :: !res;
    doprn (succ j) (succ j)

  in doprn 0 0
;;
