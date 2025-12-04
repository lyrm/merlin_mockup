let stupid_work () =
  List.init 50 (fun _ -> Random.int 100) |> List.fold_left ( + ) 0

let domain_name () =
  if Domain.is_main_domain () then "[Main]" else "      [Typer]"

let log lvl (fmt : ('a, Format.formatter, unit, unit) format4) : 'a =
  if Debug.debug_lvl > lvl then (
    Format.eprintf "%s "(domain_name ());
    Format.eprintf (fmt ^^ "\n%!"))
  else Format.ifprintf Format.std_formatter fmt
