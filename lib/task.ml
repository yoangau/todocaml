type t = Task_t.task

let complete_task (task : t) =
  let now = Datetime.iso_datetime_now () in
  let completedAt =
    match task.completedAt with
    | None -> Some now
    | Some _ -> None
  in
  { task with completedAt; modifiedAt = now }
;;

let create_task title id : t =
  let createdAt = Datetime.iso_datetime_now () in
  { id;
    title;
    description = "";
    createdAt;
    modifiedAt = createdAt;
    completedAt = None;
    deletedAt = None
  }
;;

let update_task_title (task : t) title =
  let modifiedAt = Datetime.iso_datetime_now () in
  { task with modifiedAt; title }
;;
