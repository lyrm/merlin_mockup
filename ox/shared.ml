open Await
module Lock = Capsule.Mutex.Create ()

type k = Lock.k
type mutex = k Mutex.t

type 'a t : value mod contended portable = {
  msg : (Msg.t option ref, Lock.k) Capsule.Data.t;
  data : ('a option ref, Lock.k) Capsule.Data.t;
  mutex : mutex;
  cond : Lock.k Mutex.Condition.t;
}
(* Pour DESIGN 1 >
   data: vérifier qu'on a besoin d'encapsuler dans une ref. *)

let global_mutex = Lock.mutex
let global_cond = Mutex.Condition.create ()

let create () =
  let data = Capsule.Data.create (fun () -> (ref None, ref None)) in
  {
    msg = Capsule.Data.fst data;
    data = Capsule.Data.snd data;
    mutex = global_mutex;
    cond = global_cond;
  }

let mutex _ = global_mutex
let data t = t.data

let project t ~f =
  let data =
    (Await_blocking.with_await Terminator.never ~f:(fun await ->
         Mutex.with_password await t.mutex ~f:(fun password ->
             let aliased =
               Capsule_expert.Data.map ~password ~f:(fun x -> ref (f !x)) t.data
             in
             { Modes.Aliased.aliased })))
      .aliased
  in
  { t with data }

let send_and_wait t msg =
  Await_blocking.with_await Terminator.never ~f:(fun await ->
      Mutex.with_key await t.mutex ~f:(fun key ->
          let #((), key) =
            Capsule.Expert.Key.access key ~f:(fun access ->
                let value = Capsule.Data.unwrap ~access t.msg in
                value := Some msg)
          in
          Mutex.Condition.signal t.cond;
          let key = Mutex.Condition.wait await t.cond ~lock:t.mutex key in
          Capsule.Expert.Key.access key ~f:(fun access ->
              assert (!(Capsule.Data.unwrap ~access t.msg) = None)))
      [@nontail])

let recv_clear t =
  Await_blocking.with_await Terminator.never ~f:(fun await ->
      (Mutex.with_key await t.mutex ~f:(fun key ->
           let #(value, key) =
             let rec loop key =
               let #(value, key) =
                 Capsule.Expert.Key.access key ~f:(fun access ->
                     {
                       Modes.Aliased.aliased =
                         !(Capsule.Data.unwrap ~access t.msg);
                     })
               in
               match value.aliased with
               | None ->
                   let key =
                     Mutex.Condition.wait await t.cond ~lock:t.mutex key
                   in
                   loop key
               | Some value -> #({ Modes.Aliased.aliased = value }, key)
             in
             loop key
           in
           let #((), key) =
             Capsule.Expert.Key.access key ~f:(fun access ->
                 Capsule.Data.unwrap ~access t.msg := None)
           in
           Mutex.Condition.signal t.cond;
           #(value, key)))
        .aliased)

let protected_apply t (f : _ -> _ -> ('b : immutable_data)) =
  let { Modes.Aliased.aliased; _ } =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await t.mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                let pipeline = Capsule.Data.unwrap ~access t.data in
                let msg = Capsule.Data.unwrap ~access t.msg in
                let result = f !msg !pipeline in
                { Modes.Aliased.aliased = result })))
  in
  aliased

let protected_set t f =
  Await_blocking.with_await Terminator.never ~f:(fun await ->
      Mutex.with_key await t.mutex ~f:(fun key ->
          Capsule.Expert.Key.access key ~f:(fun access ->
              let data = Capsule.Data.unwrap ~access t.data in
              let msg = Capsule.Data.unwrap ~access t.msg in
              data := f !msg)))

let inject_capsule capsule ~within:t ~(f : _ -> _ -> _ -> ('c : immutable_data))
    =
  let { Modes.Aliased.aliased; _ } =
    Await_blocking.with_await Terminator.never ~f:(fun await ->
        Mutex.with_key await global_mutex ~f:(fun key ->
            Capsule.Expert.Key.access key ~f:(fun access ->
                let msg = Capsule.Data.unwrap ~access t.msg in
                let capsule = Capsule.Data.unwrap ~access capsule in
                let within = Capsule.Data.unwrap ~access t.data in
                { Modes.Aliased.aliased = f !msg capsule !within })))
  in
  aliased
