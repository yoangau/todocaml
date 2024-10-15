val print_fresh : string -> unit
val read_line : unit -> string option
val read_single_char : unit -> char

type arrow_key =
  | Up
  | Down
  | Left
  | Right

val read_arrow_key : unit -> arrow_key option
