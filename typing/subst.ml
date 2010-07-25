open Types
open Module

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

let type_constructor s r =
  if r.ref_id.id_module = s.subst_module then
    { ref_id = r.ref_id;
      ref_contents = Some (List.assq (get_type_constr r) s.types) }
  else
    r

let mkgenty desc = { desc = desc; level = generic }

let rec core_type s ty =
  let ty = Btype.repr ty in
  begin match ty.desc with
    | Tvar _ -> ty
    | Tarrow (tyl, tyr) -> mkgenty(Tarrow (core_type s tyl, core_type s tyr))
    | Tproduct l -> mkgenty(Tproduct (List.map (core_type s) l))
    | Tconstr (c, l) -> mkgenty(Tconstr (type_constructor s c, List.map (core_type s)l ))
  end

let type_list s tyl = List.map (core_type s) tyl
