let print_arr_int ?(prefix = "a") arr =
  Format.printf "%s [@[<hov>%a@]]@." prefix
    (Format.(pp_print_array ~pp_sep:(fun out () -> fprintf out ";@ "))
       Format.pp_print_int)
    arr

let stupid_work () =
  List.init 100000 (fun _ -> Random.int 1000) |> List.fold_left ( + ) 0

let string_domain () =
  if Domain.is_main_domain () then "Main - " else "      Typer - "
