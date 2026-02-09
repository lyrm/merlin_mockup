module Unix = UnixLabels

let log fmt = Log.debug 0 ("Server : " ^^ fmt)

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
  log "read request";
  Bytes.sub buf 0 read |> String.of_bytes

let respond socket resp =
  Unix.send socket ~buf:(Bytes.of_string resp) ~pos:0 ~len:(String.length resp)
    ~mode:[]
  |> ignore;
  log "respond written";
  Unix.close socket

let listen ~handle =
  let socket =
    Unix.socket ~cloexec:true ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0
  in
  Unix.setsockopt socket SO_REUSEADDR true;
  Unix.setsockopt socket SO_REUSEPORT true;
  let port = 8453 in
  let addr = Unix.(ADDR_INET (Unix.inet_addr_loopback, port)) in
  Unix.bind socket ~addr;
  Unix.listen socket ~max:5;
  log "listening on localhost:%i" port;
  try
    while true do
      let client, _client_addr = Unix.accept socket in
      match read_request client |> parse_request with
      | None -> log "malformed response"
      | Some req -> respond client (handle req)
    done;
    Unix.close socket
  with Unix.Unix_error (EINTR, _, _) ->
    log "Interrupted" (* Graceful termination. *)
