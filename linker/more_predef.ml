open Types;;
let predef_variables = [
Pdot("meta", "global_data");
Pdot("sys", "s_irusr");
Pdot("sys", "s_iwusr");
Pdot("sys", "s_ixusr");
Pdot("sys", "s_irgrp");
Pdot("sys", "s_iwgrp");
Pdot("sys", "s_ixgrp");
Pdot("sys", "s_iroth");
Pdot("sys", "s_iwoth");
Pdot("sys", "s_ixoth");
Pdot("sys", "s_isuid");
Pdot("sys", "s_isgid");
Pdot("sys", "s_irall");
Pdot("sys", "s_iwall");
Pdot("sys", "s_ixall");
Pdot("sys", "command_line");
Pdot("sys", "interactive");
Pdot("sys", "max_string_length");
Pdot("sys", "max_vect_length");
Pdot("sys", "word_size")
];;
let predef_exn = [
(Pdot("exc", "Out_of_memory"), 1);
(Pdot("sys", "Sys_error"), 1);
(Pdot("exc", "Failure"), 3);
(Pdot("exc", "Invalid_argument"), 2);
(Pdot("io", "End_of_file"), 1);
(Pdot("int", "Division_by_zero"), 1);
(Pdot("sys", "Break"), 2);
(Pdot("exc", "Not_found"), 4);
(Pdot("unix", "Unix_error"), 1);
(Pdot("graphics", "Graphic_failure"), 1);
(Pdot("stream", "Parse_failure"), 1);
(Pdot("protocol", "TkError"), 1)
];;
