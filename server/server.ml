module Unix = UnixLabels

let log fmt =
  let domain_name = if Domain.is_main_domain () then "Main" else "Typer" in
  Format.eprintf ("%s : " ^^ fmt ^^ "\n%!") domain_name
;;

let setup_server ~socket_fname =
  let socket = Unix.socket ~cloexec:true ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 in
  let addr = Unix.ADDR_UNIX socket_fname in
  Unix.bind socket ~addr;
  Unix.listen socket ~max:5;
  log "server is setup";
  socket
;;

let read_request socket =
  let buf = Bytes.create 10 in
  let read = Unix.recv socket ~buf ~pos:0 ~len:(Bytes.length buf) ~mode:[] in
  Bytes.sub buf 0 read |> String.of_bytes
;;

let respond socket s =
  Unix.send socket ~buf:(Bytes.of_string s) ~pos:0 ~len:(String.length s) ~mode:[]
  |> ignore
;;

module Shared = struct
  type 'a t =
    { requests : 'a Queue.t
    ; mutex : Mutex.t
    ; cond : Condition.t
    }

  let create () =
    { requests = Queue.create (); mutex = Mutex.create (); cond = Condition.create () }
  ;;

  let push v s =
    Mutex.protect s.mutex (fun () ->
      let was_empty = Queue.is_empty s.requests in
      Queue.add v s.requests;
      if was_empty then Condition.broadcast s.cond)
  ;;

  let take s =
    Mutex.protect s.mutex (fun () ->
      while Queue.is_empty s.requests do
        Condition.wait s.cond s.mutex
      done;
      Queue.take s.requests)
  ;;
end

let listen shared =
  let server = setup_server ~socket_fname:Sys.argv.(1) in
  while true do
    let client, _client_addr = Unix.accept server in
    match read_request client |> int_of_string_opt with
    | None -> log "ignoring malformed request"
    | Some req ->
      log "request: %i" req;
      Shared.push (client, req) shared
  done
;;

let work shared =
  while true do
    let socket, request = Shared.take shared in
    let response = string_of_int (request * 10) in
    respond socket response;
    log "respond %S" response;
    Unix.close socket
  done
;;

(* Clean up the socket once the server is shutdown. *)
let () = Sys.set_signal Sys.sigint (Signal_handle (fun _ -> Unix.unlink Sys.argv.(1)))

let () =
  let requests = Shared.create () in
  let worker = Domain.spawn (fun () -> work requests) in
  listen requests;
  Domain.join worker
;;
