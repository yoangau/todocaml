let storage_dir = "data"
let storage_file = storage_dir ^ "/tasks.json"

let read_file filename =
  let channel = open_in filename in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    content
  with
  | e ->
    close_in channel;
    raise e
;;

let write_file_content filename content =
  let channel = open_out filename in
  try
    output_string channel content;
    close_out channel
  with
  | e ->
    close_out channel;
    raise e
;;

let save_tasks tasks = write_file_content storage_file (Task_j.string_of_tasks { tasks })
let create_empty_file () = save_tasks []

let load_tasks () : Task.t list =
  if not (Sys.file_exists storage_dir) then Sys.mkdir storage_dir 0o777;
  if not (Sys.file_exists storage_file) then create_empty_file ();
  (Task_j.tasks_of_string (read_file storage_file)).tasks
;;
