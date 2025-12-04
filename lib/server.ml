module Unix = UnixLabels

let log fmt = Utils.log 0 ("Server : " ^^ fmt)

let setup_server ~socket_fname =
  let socket =
    Unix.socket ~cloexec:true ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0
  in
  let addr = Unix.ADDR_UNIX socket_fname in
  Unix.bind socket ~addr;
  Unix.listen socket ~max:5;
  log "server is setup";
  socket

let parse_request req =
  match String.split_on_char '\n' req with
  | [ source; compl ] ->
      if compl = "all" then Some { Moconfig.source; completion = All }
      else
        Scanf.sscanf_opt compl "part %i" Fun.id
        |> Option.map (fun n -> { Moconfig.source; completion = Part n })
  | _ -> None

let read_request socket =
  let buf = Bytes.create 1000 in
  let read = Unix.recv socket ~buf ~pos:0 ~len:(Bytes.length buf) ~mode:[] in
  let req = Bytes.sub buf 0 read |> String.of_bytes in
  log "IO read request %S" req;
  req

let respond socket resp =
  Unix.send socket ~buf:(Bytes.of_string resp) ~pos:0 ~len:(String.length resp)
    ~mode:[]
  |> ignore;
  log "IO: write response %S" resp;
  Unix.close socket

let listen ~socket_fname ~handle =
  let server = setup_server ~socket_fname in
  try
    while true do
      let client, _client_addr = Unix.accept server in
      match read_request client |> parse_request with
      | None -> log "malformed response"
      | Some req -> respond client (handle req)
    done
  with Unix.Unix_error (EINTR, _, _) ->
    (* Graceful termination. *)
    log "Interrupted"
