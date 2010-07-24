open List
open Syntaxe

type valeur =
     Val_nombre of int
   | Val_bool�enne of bool
   | Val_paire of valeur * valeur
   | Val_nil
   | Val_cons of valeur * valeur
   | Val_fermeture of fermeture
   | Val_primitive of (valeur -> valeur)

and fermeture =
  { d�finition: (motif * expression) list;
    mutable environnement: environnement }

and environnement = (string * valeur) list;;

exception Erreur of string;;
exception �chec_filtrage;;

let rec filtrage valeur motif =
  match (valeur, motif) with
  | (v, Motif_variable id) -> [id, v]
  | (Val_bool�enne b1, Motif_bool�en b2) ->
      if b1 = b2 then [] else raise �chec_filtrage
  | (Val_nombre i1, Motif_nombre i2) ->
      if i1 = i2 then [] else raise �chec_filtrage
  | (Val_paire(v1, v2), Motif_paire(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (Val_nil, Motif_nil) -> []
  | (Val_cons(v1, v2), Motif_cons(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (_, _) -> raise �chec_filtrage;;
let rec �value env expr =
  match expr with
  | Variable id ->
      begin try
        assoc id env
      with Not_found -> raise(Erreur(id ^ " est inconnu"))
      end
  | Fonction(liste_de_cas) ->
      Val_fermeture {d�finition = liste_de_cas; environnement = env}
  | Application(fonction, argument) ->
      let val_fonction = �value env fonction in
      let val_argument = �value env argument in
      begin match val_fonction with
      | Val_primitive fonction_primitive ->
          fonction_primitive val_argument
      | Val_fermeture fermeture ->
          �value_application fermeture.environnement
                             fermeture.d�finition val_argument
      | _ ->
          raise(Erreur "application d'une valeur non fonctionnelle")
      end
  | Let(d�finition, corps) ->
      �value (�value_d�finition env d�finition) corps
  | Bool�en b -> Val_bool�enne b
  | Nombre n -> Val_nombre n
  | Paire(e1, e2) -> Val_paire(�value env e1, �value env e2)
  | Nil -> Val_nil
  | Cons(e1, e2) -> Val_cons(�value env e1, �value env e2)

and �value_application env liste_de_cas argument =
  match liste_de_cas with
  | [] -> raise(Erreur "�chec du filtrage")
  | (motif, expr) :: autres_cas ->
      try
        let env_�tendu = filtrage argument motif @ env in
        �value env_�tendu expr
      with �chec_filtrage ->
        �value_application env autres_cas argument

and �value_d�finition env_courant d�f =
  match d�f.r�cursive with
  | false -> (d�f.nom, �value env_courant d�f.expr) :: env_courant
  | true ->
      match d�f.expr with
      | Fonction liste_de_cas ->
          let fermeture =
            { d�finition = liste_de_cas; environnement = [] } in
          let env_�tendu =
            (d�f.nom, Val_fermeture fermeture) :: env_courant in
          fermeture.environnement <- env_�tendu;
          env_�tendu
      | _ -> raise(Erreur "let rec non fonctionnel");;
let rec imprime_valeur = function
  | Val_nombre n -> print_int n
  | Val_bool�enne false -> print_string "false"
  | Val_bool�enne true -> print_string "true"
  | Val_paire (v1, v2) ->
      print_string "("; imprime_valeur v1;
      print_string ", "; imprime_valeur v2;
      print_string ")"
  | Val_nil ->
      print_string "[]"
  | Val_cons (v1, v2) ->
      imprime_valeur v1;
      print_string "::"; imprime_valeur v2
  | Val_fermeture _ | Val_primitive _ ->
      print_string "<fun>";;
