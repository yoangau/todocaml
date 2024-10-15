let print_fresh text =
  ignore (Sys.command "clear");
  print_endline text
;;

let set_non_canonical_mode () =
  let open Unix in
  let termios = tcgetattr stdin in
  let new_termios = { termios with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSANOW new_termios
;;

let reset_terminal_mode old_termios =
  let open Unix in
  tcsetattr stdin TCSANOW old_termios
;;

let read_single_char () =
  let open Unix in
  let old_termios = tcgetattr stdin in
  set_non_canonical_mode ();
  let ch = input_char In_channel.stdin in
  reset_terminal_mode old_termios;
  ch
;;

let read_line () = In_channel.input_line In_channel.stdin

type arrow_key =
  | Up
  | Down
  | Left
  | Right

let read_arrow_key () =
  let read_escape_sequence () =
    match input_char stdin with
    | '[' ->
      (* Start of arrow key sequence *)
      (match input_char stdin with
       | 'A' -> Some Up
       | 'B' -> Some Down
       | 'C' -> Some Right
       | 'D' -> Some Left
       | _ -> None)
    | _ -> None
  in
  read_escape_sequence ()
;;
