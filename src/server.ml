module Unix = UnixLabels

let log fmt = Log.debug 0 ("Server : " ^^ fmt)

type request = Config of Moconfig.t | Close
type t = Request of request | Bad_request

let parse_request req =
  match String.split_on_char '\n' req with
  | [ source; "all" ] -> Request (Config { Moconfig.source; completion = All })
  | [ source; "close" ] -> Request Close
  | [ source; compl ] -> begin
      match Scanf.sscanf_opt compl "part %i" Fun.id with
      | None -> Bad_request
      | Some n -> Request (Config { Moconfig.source; completion = Part n })
    end
  | _ -> Bad_request

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
  let rec loop () =
    let client, _client_addr = Unix.accept socket in
    match read_request client |> parse_request with
    | Bad_request ->
        log "malformed response";
        loop ()
    | Request req ->
        respond client (handle req);
        loop ()
  in
  (try loop () with Unix.Unix_error (EINTR, _, _) -> log "Interrupted");
  Unix.close socket
