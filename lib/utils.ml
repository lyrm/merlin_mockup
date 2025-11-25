let stupid_work () =
  List.init 50 (fun _ -> Random.int 100) |> List.fold_left ( + ) 0

let domain_name () =
  if Domain.is_main_domain () then "[Main]" else "      [Typer]"

let log fmt =
  Format.eprintf "%s " (domain_name ());
  Format.eprintf (fmt ^^ "\n%!")
