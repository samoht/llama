open Lambda

let transl_module modenv m =
  assert (m <> Modenv.current_module modenv);
  Lprim (Pgetglobal (Makeident.of_module m), [])

let transl_exception modenv cs =
  let m = cs.Base.cs_module in
  if m = Modenv.current_module modenv then
    Lvar (Makeident.of_exception cs)
  else if m = Base.Module_builtin then
    Lprim (Pgetglobal (Makeident.of_exception cs), [])
  else
    let pos = Modenv.lookup_exception_position modenv cs in
    Lprim (Pfield pos, [transl_module modenv m])

let transl_value modenv v =
  let m = v.Base.val_module in
  if m = Base.Module_builtin || m = Modenv.current_module modenv then
    let id = Makeident.of_value v in
    Lvar id
  else
    let pos = Modenv.lookup_value_position modenv v in
    Lprim(Pfield pos, [transl_module modenv m])
