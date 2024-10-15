type t = Task.t list
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

let with_message message state = { state with message = Some message }
let clear_message state = { state with message = None }

let get_tasks { show_completed; tasks; _ } =
  if show_completed
  then tasks
  else List.filter (fun (task : Task.t) -> Option.is_none task.completedAt) tasks
;;

let add_task title state =
  { state with tasks = Task.create_task title (List.length state.tasks) :: state.tasks }
;;

let init tasks =
  { status = Running; tasks; cursor = 0; message = None; show_completed = true }
;;

let update task state =
  let tasks =
    List.map
      (fun original_task ->
        if task.Task_t.id == original_task.Task_t.id then task else original_task)
      state.tasks
  in
  { state with tasks }
;;
