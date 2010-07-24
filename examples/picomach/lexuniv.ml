open Stream

type lex�me =
     MC of string
   | Ident of string
   | Entier of int;;

let rec lire_entier accumulateur flux =
  match flux with
  | [< `('0'..'9' as c) >] ->
      lire_entier (10 * accumulateur + Char.code c - 48) flux
  | [< >] ->
      accumulateur;;

let tampon = "----------------";;

let rec lire_mot position flux =
  match flux with
  | [< `('A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' | 
         '�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|
         '�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'
         as c) >] ->
      if position < String.length tampon then
        set_nth_char tampon position c;
      lire_mot (position+1) flux
  | [< >] ->
      String.sub tampon 0 (min position (String.length tampon));;
let rec lire_symbole position flux =
  match flux with
  | [< `('!'|'$'|'%'|'&'|'*'|'+'|'-'|'.'|'/'|':'|
         ';'|'<'|'='|'>'|'?'|'@'|'^'|'|'|'~' as c) >] ->
      if position < String.length tampon then
        set_nth_char tampon position c;
      lire_symbole (position + 1) flux
  | [< >] ->
      String.sub tampon 0 (min position (String.length tampon));;
let rec lire_commentaire flux =
  match flux with
  | [< `'\n' >] -> ()
  | [< `c >] -> lire_commentaire flux;;
let mc_ou_ident table_des_mots_cl�s ident =
    try Hashtbl.find table_des_mots_cl�s ident
    with Not_found -> Ident(ident);;
let mc_ou_erreur table_des_mots_cl�s caract�re =
    let ident = String.make 1 caract�re in
    try Hashtbl.find table_des_mots_cl�s ident
    with Not_found -> raise Parse_error;;
let rec lire_lex�me table flux =
  match flux with
  | [< `(' '|'\n'|'\r'|'\t') >] ->
      lire_lex�me table flux
  | [< `'#' >] ->
      lire_commentaire flux; lire_lex�me table flux
  | [< `('A'..'Z' | 'a'..'z' | 
         '�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|
         '�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'|'�'
         as c) >] ->
      set_nth_char tampon 0 c;
      mc_ou_ident table (lire_mot 1 flux)
  | [< `('!'|'$'|'%'|'&'|'*'|'+'|'.'|'/'|':'|';'|
         '<'|'='|'>'|'?'|'@'|'^'|'|'|'~' as c) >] ->
      set_nth_char tampon 0 c;
      mc_ou_ident table (lire_symbole 1 flux)
  | [< `('0'..'9' as c) >] ->
      Entier(lire_entier (Char.code c - 48) flux)
  | [< `'-' >] ->
      begin match flux with
      | [< `('0'..'9' as c) >] ->
          Entier(- (lire_entier  (Char.code c - 48) flux))
      | [< >] ->
          set_nth_char tampon 0 '-';
          mc_ou_ident table (lire_symbole 1 flux)
      end
  | [< `c >] ->
      mc_ou_erreur table c;;
let rec analyseur table flux =
    stream_from (function () -> 
      match flux with
      | [< (lire_lex�me table) lex�me >] -> lex�me
      | [< >] -> raise Parse_failure);;
let construire_analyseur mots_cl�s =
    let table_des_mots_cl�s = Hashtbl.create 17 in
    iter
      (function mot -> Hashtbl.add table_des_mots_cl�s mot (MC mot))
      mots_cl�s;
    analyseur table_des_mots_cl�s;;
