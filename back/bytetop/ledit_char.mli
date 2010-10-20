type encoding = Ascii | Iso_8859 | Utf_8
val encoding : encoding ref
val nbc : char -> int

type t = string
val of_ascii : char -> t
val to_ascii : t -> char option
val is_word_char : t -> bool
val ctrl_val : t -> t option
val meta_ctrl_val : t -> t option
val not_ascii_val : t -> (t * t * t) option
val uppercase : t -> t
val lowercase : t -> t
val parse : char Stream.t -> t
val to_string : t -> string
val input : in_channel -> t
val read : unit -> t
val print : t -> unit
val prerr : t -> unit
val prerr_backsp : t -> unit
