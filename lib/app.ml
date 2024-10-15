open Model

type edit_type =
  | Complete
  | Content

type top_level_command =
  | AddTask
  | Edit of edit_type
  | Quit
  | Save
  | Select of int
  | Invalid
  | ToggleView

let task_per_page = 5

let select_from_arrow_key () =
  let open Io in
  match read_arrow_key () with
  | Some Up -> Select (-1)
  | Some Down -> Select 1
  | Some Left -> Select (-task_per_page)
  | Some Right -> Select task_per_page
  | None -> Select 0
;;

let parse_top_level_command () =
  match Io.read_single_char () with
  | 'q' -> Quit
  | 'a' -> AddTask
  | 'c' -> Edit Complete
  | 'e' -> Edit Content
  | 's' -> Save
  | 't' -> ToggleView
  | '\027' -> select_from_arrow_key ()
  | _ -> Invalid
;;

let add_task app =
  Ui.task_creation_page ();
  match Io.read_line () with
  | None -> { app with status = Running }
  | Some title ->
    { app with
      tasks = Task.create_task title :: app.tasks;
      status = Running;
      message = Some (Ok "Task added successfully")
    }
;;

let quit app =
  Io.print_fresh "Quitting";
  { app with status = Terminal; message = None }
;;

let get_tasks { show_completed; tasks; _ } =
  if show_completed
  then tasks
  else
    List.filter
      (fun (task : Task.t) -> show_completed || Option.is_none task.completedAt)
      tasks
;;

let clamp_selected app selected =
  let clamp min max value = Int.min max (Int.max value min) in
  clamp 0 (List.length (get_tasks app) - 1) (app.selected + selected)
;;

let select app selected =
  { app with selected = clamp_selected app selected; status = Running; message = None }
;;

let update_task_title task =
  Ui.task_update_page task;
  Io.read_line ()
  |> Option.fold ~none:task ~some:(fun title -> Task.update_task_title task title)
;;

let edit app t =
  let selected_task = List.nth app.tasks app.selected in
  let modify_selected modified_task =
    let tasks =
      List.mapi
        (fun i original_task -> if i = app.selected then modified_task else original_task)
        app.tasks
    in
    { app with tasks; message = Some (Ok "Task sucessfully updated") }
  in
  let modified_task =
    match t with
    | Complete -> Task.complete_task selected_task
    | Content -> update_task_title selected_task
  in
  modify_selected modified_task
;;

let save app =
  Storage.save_tasks app.tasks;
  { app with message = Some (Ok "Tasks saved!") }
;;

let invalidate_command app = { app with message = Some (Error "Invalid input") }

let toggle_view app =
  let toggled_app = { app with show_completed = not app.show_completed } in
  select toggled_app toggled_app.selected
;;

let execute_top_level_command app = function
  | Quit -> quit app
  | AddTask -> add_task app
  | Edit t -> edit app t
  | Save -> save app
  | Select selected -> select app selected
  | Invalid -> invalidate_command app
  | ToggleView -> toggle_view app
;;

let init_app () =
  let tasks = Storage.load_tasks () in
  { status = Running; tasks; selected = 0; message = None; show_completed = true }
;;

let run () =
  let rec aux app =
    Ui.home_page (get_tasks app) app.selected task_per_page app.message;
    match parse_top_level_command () |> execute_top_level_command app with
    | { status = Running; _ } as next_app -> aux next_app
    | _ -> save app
  in
  let app = init_app () in
  ignore (aux app)
;;
