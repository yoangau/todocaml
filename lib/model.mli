type t
type message = (string, string) result

type status =
  | Terminal
  | Running

type state =
  { tasks : t;
    status : status;
    cursor : int;
    message : message option;
    show_completed : bool
  }

val get_tasks : state -> Task.t list
val add_task : string -> state -> state
val with_message : message -> state -> state
val clear_message : state -> state
val init : Task.t list -> state
val update : Task.t -> state -> state
