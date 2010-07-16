exception �chec;;

let reconna�t automate cha�ne =
  let �tat_courant = ref automate in 
  try
    for i = 0 to string_length cha�ne - 1 do
    match !�tat_courant.dtransitions.(int_of_char(nth_char cha�ne i)) with
    | Rejet  -> raise �chec
    | Vers e -> �tat_courant := e
    done;
    !�tat_courant.dterminal
  with �chec -> false;;
open Auto

type ensemble_d'�tats =
  { contenu  : Ensent.t;
    �l�ments : Auto.�tat list };;
let vide = { contenu = Ensent.vide; �l�ments = [] };;
let est_vide ens =
  match ens.�l�ments with [] -> true | _ -> false;;
let appartient �tat ens =
  Ensent.appartient �tat.num�ro ens.contenu;;
let ajoute �tat ens =
  { contenu  = Ensent.ajoute �tat.num�ro ens.contenu;
    �l�ments = �tat :: ens.�l�ments };;
let rec ajoute_fermeture �tat ferm =
  if appartient �tat ferm then ferm else
    list_it ajoute_fermeture
            �tat.epsilon_transitions (ajoute �tat ferm);;

let fermeture �tat = ajoute_fermeture �tat vide;;

let fermeture_ens ens = list_it ajoute_fermeture ens.�l�ments vide;;
let d�placements liste_�tats =
  let t = make_vect 256 vide in
  do_list
    (function �tat ->
      do_list
        (function (car, dest) ->
          let i = int_of_char car in t.(i) <- ajoute dest t.(i))
      �tat.transitions)
    liste_�tats;
  t;;
let d�terminise �tat_initial =
  let �tats_connus = Hashtbl.new 51
  and �_remplir = Stack.new () in
  let traduire ens =
    try Hashtbl.find �tats_connus ens.contenu
    with Not_found ->
      let nouvel_�tat =
        { dterminal = exists (function n -> n.terminal) ens.�l�ments;
          dtransitions = make_vect 256 Rejet } in
      Hashtbl.add �tats_connus ens.contenu nouvel_�tat;
      Stack.push (ens.�l�ments, nouvel_�tat) �_remplir;
      nouvel_�tat in
  let nouvel_�tat_initial =
    traduire (fermeture �tat_initial) in
  begin try
    while true do
      let (liste, nouvel_�tat) = Stack.pop �_remplir in
      let d�pl = d�placements liste in
      for i = 0 to 255 do
        if not est_vide d�pl.(i) then
          nouvel_�tat.dtransitions.(i) <-
            Vers(traduire (fermeture_ens d�pl.(i)))
      done
    done
  with Stack.Empty -> ()
  end;
  nouvel_�tat_initial;;
