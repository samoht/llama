open Types;; open Path;;  let mkpers i = Pident(Id.create_persistent i);;   
let predef_variables = [
Pdot(mkpers "meta", "global_data");
Pdot(mkpers "sys", "s_irusr");
Pdot(mkpers "sys", "s_iwusr");
Pdot(mkpers "sys", "s_ixusr");
Pdot(mkpers "sys", "s_irgrp");
Pdot(mkpers "sys", "s_iwgrp");
Pdot(mkpers "sys", "s_ixgrp");
Pdot(mkpers "sys", "s_iroth");
Pdot(mkpers "sys", "s_iwoth");
Pdot(mkpers "sys", "s_ixoth");
Pdot(mkpers "sys", "s_isuid");
Pdot(mkpers "sys", "s_isgid");
Pdot(mkpers "sys", "s_irall");
Pdot(mkpers "sys", "s_iwall");
Pdot(mkpers "sys", "s_ixall");
Pdot(mkpers "sys", "command_line");
Pdot(mkpers "sys", "interactive");
Pdot(mkpers "sys", "max_string_length");
Pdot(mkpers "sys", "max_vect_length");
Pdot(mkpers "sys", "word_size")
];;
let predef_exn = [
(Pdot(mkpers "exc", "Out_of_memory"), 1);
(Pdot(mkpers "sys", "Sys_error"), 1);
(Pdot(mkpers "exc", "Failure"), 3);
(Pdot(mkpers "exc", "Invalid_argument"), 2);
(Pdot(mkpers "io", "End_of_file"), 1);
(Pdot(mkpers "int", "Division_by_zero"), 1);
(Pdot(mkpers "sys", "Break"), 2);
(Pdot(mkpers "exc", "Not_found"), 4);
(Pdot(mkpers "unix", "Unix_error"), 1);
(Pdot(mkpers "graphics", "Graphic_failure"), 1);
(Pdot(mkpers "stream", "Parse_failure"), 1);
(Pdot(mkpers "protocol", "TkError"), 1)
];;
