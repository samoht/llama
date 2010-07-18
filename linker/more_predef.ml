open Asttypes;;
let predef_variables = [
{qual="meta"; id="global_data"};
{qual="sys"; id="s_irusr"};
{qual="sys"; id="s_iwusr"};
{qual="sys"; id="s_ixusr"};
{qual="sys"; id="s_irgrp"};
{qual="sys"; id="s_iwgrp"};
{qual="sys"; id="s_ixgrp"};
{qual="sys"; id="s_iroth"};
{qual="sys"; id="s_iwoth"};
{qual="sys"; id="s_ixoth"};
{qual="sys"; id="s_isuid"};
{qual="sys"; id="s_isgid"};
{qual="sys"; id="s_irall"};
{qual="sys"; id="s_iwall"};
{qual="sys"; id="s_ixall"};
{qual="sys"; id="command_line"};
{qual="sys"; id="interactive"};
{qual="sys"; id="max_string_length"};
{qual="sys"; id="max_vect_length"};
{qual="sys"; id="word_size"}
];;
let predef_exn = [
({qual="exc"; id="Out_of_memory"}, 1);
({qual="sys"; id="Sys_error"}, 1);
({qual="exc"; id="Failure"}, 3);
({qual="exc"; id="Invalid_argument"}, 2);
({qual="io"; id="End_of_file"}, 1);
({qual="int"; id="Division_by_zero"}, 1);
({qual="sys"; id="Break"}, 2);
({qual="exc"; id="Not_found"}, 4);
({qual="unix"; id="Unix_error"}, 1);
({qual="graphics"; id="Graphic_failure"}, 1);
({qual="stream"; id="Parse_failure"}, 1);
({qual="protocol"; id="TkError"}, 1)
];;
