module Unix = UnixLabels

let setup_server ~socket_fname =
  let socket =
    Unix.socket ~cloexec:true ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0
  in
  let addr = Unix.ADDR_UNIX socket_fname in
  Unix.bind socket ~addr;
  Unix.listen socket ~max:5;
  socket

let parse_request req =
  match String.split_on_char '\n' req with
  | [ source; "all" ] -> Some { Moconfig.source; completion = All }
  | [ source; compl ] ->
      Scanf.sscanf_opt compl "part %i" Fun.id
      |> Option.map (fun n -> { Moconfig.source; completion = Part n })
  | _ -> None

let read_request socket =
  let buf = Bytes.create 1000 in
  let read = Unix.recv socket ~buf ~pos:0 ~len:(Bytes.length buf) ~mode:[] in
  Bytes.sub buf 0 read |> String.of_bytes

let respond socket resp =
  Unix.send socket ~buf:(Bytes.of_string resp) ~pos:0 ~len:(String.length resp)
    ~mode:[]
  |> ignore;
  Unix.close socket

let listen ~socket_fname ~handle =
  let server = setup_server ~socket_fname in
  try
    while true do
      let client, _client_addr = Unix.accept server in
      match read_request client |> parse_request with
      | None -> ()
      | Some req -> respond client (handle req)
    done
  with Unix.Unix_error (EINTR, _, _) -> () (* Graceful termination. *)
