type t = Task_t.task

val complete_task : t -> t
val create_task : string -> t
val update_task_title : t -> string -> t