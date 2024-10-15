let iso_datetime_now () =
  let Unix.{ tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.(time () |> localtime)
  in
  Format.sprintf
    "%4i-%02i-%02iT%02i:%02i:%02i"
    (tm_year + 1900)
    (tm_mon + 1)
    tm_mday
    tm_hour
    tm_min
    tm_sec
;;
