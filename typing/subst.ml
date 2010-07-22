open Types

type t = {
  types : (type_constructor * type_constructor) list;
  values : (value * value) list; (* currently unused *)
}

let identity = { types = []; values = [] }

let add_type_constructor c1 c2 s = { s with types = (c1, c2) :: s.types }

let type_constructor s c =
  try List.assq c s.types with Not_found -> c

let mkgenty desc = { typ_desc = desc; typ_level = generic }

let rec core_type s ty =
  let ty = Btype.type_repr ty in
  begin match ty.typ_desc with
    | Tvar _ -> ty
    | Tarrow (tyl, tyr) -> mkgenty(Tarrow (core_type s tyl, core_type s tyr))
    | Tproduct l -> mkgenty(Tproduct (List.map (core_type s) l))
    | Tconstr (c, l) -> mkgenty(Tconstr (type_constructor s c, List.map (core_type s)l ))
  end

let type_list s tyl = List.map (core_type s) tyl
