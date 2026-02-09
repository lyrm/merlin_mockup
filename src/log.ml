let debug_lvl = 3

let domain_name () =
  if Multicore.current_domain () = 1 then "[Main]" else "  [Typer]"

let debug lvl fmt =
  if debug_lvl > lvl then begin
    Stdio.Out_channel.eprintf "%s " (domain_name ());
    Stdio.Out_channel.eprintf (fmt ^^ "\n%!")
  end
  else Base.Printf.ifprintf Stdio.stderr fmt
