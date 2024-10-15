type message = (string, string) result

type status =
  | Terminal
  | Running

type state =
  { tasks : Task_t.task list;
    status : status;
    selected : int;
    message : message option;
    show_completed : bool
  }
