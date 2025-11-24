module Unix = UnixLabels

let log fmt =
  let domain_name = if Domain.is_main_domain () then "main" else "work" in
  Format.eprintf ("Server [%s] : " ^^ fmt ^^ "\n%!") domain_name

let setup_server ~socket_fname =
  let socket =
    Unix.socket ~cloexec:true ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0
  in
  let addr = Unix.ADDR_UNIX socket_fname in
  Unix.bind socket ~addr;
  Unix.listen socket ~max:5;
  log "server is setup";
  socket

let read_request socket =
  let buf = Bytes.create 10 in
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

module Shared = struct
  type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a option }

  let create () =
    { mutex = Mutex.create (); cond = Condition.create (); value = None }

  let put t v =
    Mutex.protect t.mutex @@ fun () ->
    t.value <- Some v;
    log "put";
    Condition.signal t.cond;
    Condition.wait t.cond t.mutex

  let take t =
    Mutex.protect t.mutex @@ fun () ->
    let rec loop () =
      match t.value with
      | None ->
          Condition.wait t.cond t.mutex;
          loop ()
      | Some v -> v
    in
    let value = loop () in
    log "take";
    t.value <- None;
    Condition.signal t.cond;
    value
end

let listen shared =
  let server = setup_server ~socket_fname:Sys.argv.(1) in
  while true do
    let client, _client_addr = Unix.accept server in
    match read_request client |> int_of_string_opt with
    | None -> log "ignoring malformed request"
    | Some req ->
        Shared.put shared req;
        let response = Shared.take shared in
        respond client (string_of_int response)
  done

let work shared =
  while true do
    let request = Shared.take shared in
    Shared.put shared (request * 10)
  done

(* Clean up the socket once the server is shutdown. *)
let () =
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> Unix.unlink Sys.argv.(1)))

(* If the client closes its connection, don't let it kill us with a SIGPIPE. *)
let () = if Sys.unix then Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let () =
  let shared = Shared.create () in
  let worker = Domain.spawn (fun () -> work shared) in
  try
    let _ = listen shared in
    Domain.join worker
  with Unix.Unix_error (EINTR, _, _) ->
    (* Graceful termination. *)
    log "Interrupted"
