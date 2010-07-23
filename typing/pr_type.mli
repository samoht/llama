open Types

val output_one_type : out_channel -> core_type -> unit
val output_schema : out_channel -> core_type -> unit
val output_type : out_channel -> core_type -> unit
val reset_type_var_name : unit -> unit
val output_type_constr : out_channel -> type_constructor -> unit
val output_constr : out_channel -> constructor -> unit
val output_label : out_channel -> label -> unit
