let format_text format text = Format.sprintf "%s%s\x1b[0m" format text
let format_green = format_text "\x1b[32m"
let format_red = format_text "\x1b[31m"
let format_bold_underline = format_text "\x1b[1;4m"

let serialize_task_entry selected index ({ title; completedAt; _ } : Task_t.task) =
  let completed = Option.is_some completedAt in
  let check = if completed then format_green "✔" else "" in
  if selected = index
  then Format.sprintf "→ %i\t%s\t\t%s \n" (index + 1) check title
  else Format.sprintf "  %i\t%s\t\t%s \n" (index + 1) check title
;;

let serialize_home_help () =
  "[↑ ↓] Select Task | [← →] Change Page | [t] Toggle View | [q] Quit | [s] Save"
;;

let serialize_home_header () = "  " ^ format_bold_underline "id\tcompleted\ttitle\n"

let serialize_home_footer page total_page =
  Format.sprintf "(%i/%i)\n\n%s" (page + 1) (total_page + 1) (serialize_home_help ())
;;

let serialize_message = function
  | None -> ""
  | Some (Ok m) -> format_green ("\n\n" ^ m)
  | Some (Error m) -> format_red ("\n\n" ^ m)
;;

let serialize_task_entries tasks selected page page_size =
  let tasks_in_page =
    tasks
    |> List.to_seq
    |> Seq.mapi (fun i task -> i, task)
    |> Seq.drop (page_size * page)
    |> Seq.take page_size
  in
  tasks_in_page
  |> Seq.map (fun (i, task) -> serialize_task_entry selected i task)
  |> List.of_seq
  |> String.concat ""
;;

let home_page tasks selected page_size message =
  let page = selected / page_size in
  let total_page =
    (* David is genious he found the -1*)
    (List.length tasks - 1) / page_size
  in
  Io.print_fresh
    (serialize_home_header ()
     ^ serialize_task_entries tasks selected page page_size
     ^ serialize_home_footer page total_page
     ^ serialize_message message)
;;

let task_creation_page () = Io.print_fresh "Enter task title:\n"

let task_update_page (task : Task.t) =
  Io.print_fresh @@ Format.sprintf "Update tasks title:%s" task.title
;;
