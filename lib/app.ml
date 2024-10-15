open Model

type edit_type =
  | Complete
  | Content

type increment = Increment of int

type top_level_command =
  | AddTask
  | Edit of edit_type
  | Quit
  | Save
  | MoveCursor of increment
  | Invalid
  | ToggleView

let task_per_page = 5

let select_from_arrow_key () =
  let open Io in
  match read_arrow_key () with
  | Some Up -> MoveCursor (Increment (-1))
  | Some Down -> MoveCursor (Increment 1)
  | Some Left -> MoveCursor (Increment (-task_per_page))
  | Some Right -> MoveCursor (Increment task_per_page)
  | None -> MoveCursor (Increment 0)
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
  | None -> app
  | Some title -> app |> add_task title |> with_message (Ok "Task added successfully")
;;

let quit app = { app with status = Terminal } |> with_message (Ok "Quitting")

let move_cursor app (Increment i : increment) =
  let clamp_cursor app increment =
    let clamp min max value = Int.min max (Int.max value min) in
    clamp 0 (List.length (get_tasks app) - 1) (app.cursor + increment)
  in
  { app with cursor = clamp_cursor app i; message = None }
;;

let update_task_title task =
  Ui.task_update_page task;
  Io.read_line ()
  |> Option.fold ~none:task ~some:(fun title -> Task.update_task_title task title)
;;

let edit app edit_type =
  match get_tasks app with
  | [] -> with_message (Error "No task available :(") app
  | tasks ->
    let selected_task = List.nth tasks app.cursor in
    let modified_task =
      match edit_type with
      | Complete -> Task.complete_task selected_task
      | Content -> update_task_title selected_task
    in
    update modified_task app |> with_message (Ok "Task sucessfully updated")
;;

let save app =
  Storage.save_tasks @@ get_tasks { app with show_completed = true };
  with_message (Ok "Tasks saved!") app
;;

let invalidate_command app = { app with message = Some (Error "Invalid input") }

let toggle_view app =
  let toggled_app = { app with show_completed = not app.show_completed } in
  move_cursor toggled_app (Increment 0)
;;

let execute_top_level_command app = function
  | Quit -> quit app
  | AddTask -> add_task app
  | Edit edit_type -> edit app edit_type
  | Save -> save app
  | MoveCursor selected -> move_cursor app selected
  | Invalid -> invalidate_command app
  | ToggleView -> toggle_view app
;;

let init_app () = Storage.load_tasks () |> init

let run () =
  let rec aux app =
    Ui.home_page (get_tasks app) app.cursor task_per_page app.message;
    match parse_top_level_command () |> execute_top_level_command app with
    | { status = Running; _ } as next_app -> aux next_app
    | _ -> save app
  in
  let app = init_app () in
  ignore (aux app)
;;
