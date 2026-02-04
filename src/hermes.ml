open Await
module Lock = Capsule.Mutex.Create ()

type k = Lock.k
type mutex = k Mutex.t

type 'a t : value mod contended portable = {
  msg : (Msg.t ref, k) Capsule.Data.t;
  data : ('a ref, k) Capsule.Data.t;
}

let global_mutex = Lock.mutex
let global_cond = Mutex.Condition.create ()

let create f =
  let data = Capsule.Data.create (fun () -> (ref Msg.Empty, ref (f ()))) in
  { msg = Capsule.Data.fst data; data = Capsule.Data.snd data }

let create_from t capsule ~f =
  let data =
    (Mutex.with_password (Await_blocking.await Terminator.never) global_mutex
       ~f:(fun password ->
         let aliased =
           Capsule_expert.Data.map ~password ~f:(fun x -> ref (f x)) capsule
         in
         { Modes.Aliased.aliased }))
      .aliased
  in
  { t with data }

let send_and_wait t msg =
  let await = Await_blocking.await Terminator.never in
  Mutex.with_key await global_mutex ~f:(fun key ->
      let #((), key) =
        Capsule.Expert.Key.access key ~f:(fun access ->
            let value = Capsule.Data.unwrap ~access t.msg in
            value := msg)
      in
      Mutex.Condition.signal global_cond;
      let key = Mutex.Condition.wait await global_cond ~lock:global_mutex key in
      let rec loop key =
        let #(msg, key) =
          Capsule.Expert.Key.access key ~f:(fun access ->
              { Modes.Aliased.aliased = !(Capsule.Data.unwrap ~access t.msg) })
        in
        match msg.aliased with
        | Empty -> #((), key)
        | _ ->
            let key =
              Mutex.Condition.wait await global_cond ~lock:global_mutex key
            in
            loop key
      in
      loop key [@nontail])

let recv_clear t =
  let await = Await_blocking.await Terminator.never in
  (Mutex.with_key await global_mutex ~f:(fun key ->
       let #(msg, key) =
         let rec loop key =
           let #(msg, key) =
             Capsule.Expert.Key.access key ~f:(fun access ->
                 {
                   Modes.Aliased.aliased = !(Capsule.Data.unwrap ~access t.msg);
                 })
           in
           match msg.aliased with
           | Empty ->
               let key =
                 Mutex.Condition.wait await global_cond ~lock:global_mutex key
               in
               loop key
           | _ -> #(msg, key)
         in
         loop key
       in
       let #((), key) =
         Capsule.Expert.Key.access key ~f:(fun access ->
             Capsule.Data.unwrap ~access t.msg := Empty)
       in
       Mutex.Condition.signal global_cond;
       #(msg, key)))
    .aliased

let apply t ~(f : _ -> _ -> ('b : immutable_data)) =
  let { Modes.Aliased.aliased; _ } =
    Mutex.with_access (Await_blocking.await Terminator.never) global_mutex
      ~f:(fun access ->
        let pipeline = Capsule.Data.unwrap ~access t.data in
        let msg = Capsule.Data.unwrap ~access t.msg in
        let result = f !msg !pipeline in
        { Modes.Aliased.aliased = result })
  in
  aliased

let map t ~f =
  let data =
    (Mutex.with_password (Await_blocking.await Terminator.never) global_mutex
       ~f:(fun password ->
         let aliased =
           Capsule_expert.Data.map ~password ~f:(fun x -> ref (f !x)) t.data
         in
         { Modes.Aliased.aliased }))
      .aliased
  in
  { t with data }

let apply_with_capsule capsule t ~(f : _ -> _ -> _ -> ('c : immutable_data)) =
  let { Modes.Aliased.aliased; _ } =
    Mutex.with_access (Await_blocking.await Terminator.never) global_mutex
      ~f:(fun access ->
        let msg = Capsule.Data.unwrap ~access t.msg in
        let capsule = Capsule.Data.unwrap ~access capsule in
        let within = Capsule.Data.unwrap ~access t.data in
        { Modes.Aliased.aliased = f !msg capsule !within })
  in
  aliased

let merge :
    ('a : value mod portable) ('b : value mod portable).
    'a t -> within:'b t -> f:('a -> 'b -> 'b) @ portable -> unit =
 fun t ~within ~f ->
  Mutex.with_access (Await_blocking.await Terminator.never) global_mutex
    ~f:(fun access ->
      let data = !(Capsule.Data.unwrap ~access t.data) in
      let within = Capsule.Data.unwrap ~access within.data in
      within := f data !within)

let protect_capsule (capsule : ('a, k) Capsule.Data.t) ~(f : 'a -> 'b) :
    ('b : immutable_data) =
  let { Modes.Aliased.aliased; _ } =
    Mutex.with_access (Await_blocking.await Terminator.never) global_mutex
      ~f:(fun access ->
        let data = Capsule.Data.unwrap ~access capsule in
        { Modes.Aliased.aliased = f data })
  in
  aliased
