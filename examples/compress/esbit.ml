type tampon = { mutable v: int; mutable nbits: int };;
let tampon = { v = 0; nbits = 0 };;
let initialise () = tampon.v <- 0; tampon.nbits <- 0;;
let écrire_bit sortie bit =
  tampon.v <- tampon.v lor (bit lsl tampon.nbits);
  tampon.nbits <- tampon.nbits + 1;
  if tampon.nbits >= 8 then begin
    output_char sortie (char_of_int tampon.v);
    tampon.v <- 0;
    tampon.nbits <- 0
  end;;

let finir sortie =
  if tampon.nbits > 0 then
    output_char sortie (char_of_int tampon.v);;
let lire_bit entrée =
  if tampon.nbits <= 0 then begin
    tampon.v <- int_of_char(input_char entrée);
    tampon.nbits <- 8
  end;
  let res = tampon.v land 1 in
  tampon.v <- tampon.v lsr 1;
  tampon.nbits <- tampon.nbits - 1;
  res;;
