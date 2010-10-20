exception Error

let error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Error
