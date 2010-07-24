open Code

type �tat_de_l'assembleur =
   { mutable pc: int;
     mutable code: instruction vect;
     mutable table_�tiq: (string, int) Hashtbl.t;
     mutable �_r�soudre: (int * string) list };;

exception Erreur of string;;

let asm =
  { pc = 0; code = [||]; table_�tiq = Hashtbl.create 0;
    �_r�soudre = [] };;

let initialise () =
    asm.pc <- 0;
    asm.code <- Array.create 100 Stop;
    asm.table_�tiq <- Hashtbl.create 17;
    asm.�_r�soudre <- [];;

let d�code_adresse adr = adr / taille_du_mot;;

let assemble instruction =
    if asm.pc >= Array.length asm.code then begin
      let nouveau_code = Array.create (2 * Array.length asm.code) Stop in
      blit_vect asm.code 0 nouveau_code 0 (Array.length asm.code);
      asm.code <- nouveau_code
    end;
    asm.code.(d�code_adresse asm.pc) <- instruction;
    asm.pc <- asm.pc + taille_du_mot;;

let d�finir_�tiquette nom_�tiq val_�tiq =
    try
      let d�j�_d�finie = Hashtbl.find asm.table_�tiq nom_�tiq in
      raise (Erreur ("�tiquette " ^ nom_�tiq ^ " red�finie"))
    with Not_found ->
      Hashtbl.add asm.table_�tiq nom_�tiq val_�tiq;;

let poser_�tiquette nom_�tiq =
    d�finir_�tiquette nom_�tiq asm.pc;;

let valeur_�tiquette nom_�tiq =
    try
       Hashtbl.find asm.table_�tiq nom_�tiq
    with Not_found ->
       asm.�_r�soudre <- (asm.pc, nom_�tiq) :: asm.�_r�soudre;
       0;;
let r�soudre_�tiquette (adresse, nom_�tiq) =
    let valeur =
        try
          Hashtbl.find asm.table_�tiq nom_�tiq
        with Not_found ->
          raise (Erreur ("�tiquette " ^ nom_�tiq ^ " ind�finie")) in
    let nouvelle_instruction =
        match asm.code.(d�code_adresse adresse) with
        | Op(op�ration, reg1, _, reg2) ->
            Op(op�ration, reg1, Imm valeur, reg2)
        | Jmp(_, reg) ->
            Jmp(Imm valeur, reg)
        | Braz(reg, _) ->
            Braz(reg, valeur)
        | Branz(reg, _) ->
            Branz(reg, valeur)
        | _ -> raise (Erreur "r�soudre_�tiquette") in
    asm.code.(d�code_adresse adresse) <- nouvelle_instruction;;

let extraire_code () =
    iter r�soudre_�tiquette asm.�_r�soudre;
    sub_vect asm.code 0 (d�code_adresse asm.pc);;
