open Types

type t = {
  subst_module : module_id;
  types : (type_constructor * type_constructor) list;
  values : (value * value) list; (* currently unused *)
}

let identity m = { subst_module = m; types = []; values = [] }

let add_type_constructor c1 c2 s =
  { subst_module = s.subst_module;
    types = (c1, c2) :: s.types;
    values = s.values; }

let type_constructor s tcs =
  if tcs.tcs_module = s.subst_module then
    List.assq tcs s.types
  else
    tcs

let rec core_type s ty =
  begin match ty with
    | Tvar _ -> ty
    | Tarrow (tyl, tyr) -> Tarrow (core_type s tyl, core_type s tyr)
    | Ttuple l -> Ttuple (List.map (core_type s) l)
    | Tconstr (c, l) -> Tconstr (type_constructor s c, List.map (core_type s)l)
  end

let type_list s tyl = List.map (core_type s) tyl
