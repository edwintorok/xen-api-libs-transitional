let src = Logs.Src.create "resources" ~doc:"logs from Xapi_stdext_pervasives.Resources"

module Ring = struct
  type 'a t =
    { ring: 'a option array
    ; mutable pos: int
    }

  let create n = { ring = Array.make n None; pos = 0 }
  let add t e =
    if Array.length t.ring > 0 then begin
      t.ring.(t.pos) <- Some e;
      t.pos <- (t.pos + 1) mod (Array.length t.ring)
    end

  (* iterate in reverse order *)
  let iter f t =
    let rec loop i =
      let i = (Array.length t.ring + i) mod Array.length t.ring in
      if i = t.pos then ()
      else match t.ring.(i) with
        | None -> ()
        | Some v ->
          f v; loop (i-1)
    in
    loop (t.pos-1)
end

let log_ring_size = ref 10


module Logging = struct
  module Entry = struct
    type t = { timestamp: float
             ; operation: string
             ; where: string
             }

    let create ?(loc="?") operation =
      let timestamp = Unix.gettimeofday () in
      { timestamp; operation; where = loc }

    let pp ppf t =
      let ts = Ptime.of_float_s t.timestamp in
      let pp_ptime = Ptime.pp_human ~frac_s:6 () in
      Fmt.pf ppf "%s (%s) at %a" t.operation t.where Fmt.(option pp_ptime) ts
  end

  type 'a t =
    { v: 'a
    ; allocated: Entry.t
    ; history: Entry.t Ring.t
    ; pp: 'a Fmt.t
    }

  let create ?loc ~pp f =
    let allocated = Entry.create ?loc "allocated" in
    { v = f (); allocated; history = Ring.create !log_ring_size; pp }

  let pp ppf t =
    Fmt.quote t.pp ppf t.v;
    Fmt.sp ppf ();
    Entry.pp ppf t.allocated;
    Fmt.sp ppf ();
    Fmt.iter Ring.iter Entry.pp ppf t.history;
    Format.pp_close_box ppf ()

  (* log operation to history ring buffer, or
   * log the exception from [f] and dump the ring buffer history *)
  let with_operation ?loc op t f =
    let e = Entry.create ?loc op in
    match f t.v with
    | r -> Ring.add t.history e; r
    | exception e ->
      Backtrace.is_important e;
      Logs.warn ~src (fun m -> m "%s raised %s on %a" op (Printexc.to_string e) pp t);
      raise e
end
(*
let run () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level  (Some Logs.Debug);
  Logs.debug ~src (fun m -> m "start");
  let t = Logging.create ~loc:__LOC__ ~pp:Format.pp_print_int (fun () -> 42) in
  let _ = Logging.with_operation ~loc:__LOC__ "op1" t (fun _ -> 42) in
  let _ = Logging.with_operation ~loc:__LOC__ "op1" t (fun _ -> failwith "HERE") in
  ()
*)


(*
(** At the lowest level we have resources with an associated destructor.
 * Using Rust terminology we call this 'drop': typically closing, deallocating, or otherwise
 * releasing a resource such as a file descriptor or a lock.
 * Also called a destructor in other languages.
 * *)
module type Dropable = sig
  (** a resource *)
  type t

  (** [drop resource] releases [resource]. E.g. close a file, unlock a mutex, etc.
   * Calling this multiple times may result in undefined behaviour.
   * Using [t] after it has been [drop]ed can also result in undefined behaviour
   * (e.g. file descriptor could've been already reused for another file)
   * *)
  val drop: t -> unit
end

module StackOwner = struct
  type t = (unit -> unit) Stack.t

  let add t o = Stack.push o t

  let exec f = f ()

  let drop t =
    Stack.iter exec t
end

module Owner = struct
  type t

  let pp _ppf _ = ()

  let add _ _ = ()
end

let pp_exception ppf e =
  let bt = e |> Backtrace.get |> Backtrace.to_string_hum in
  Format.fprintf ppf "%s at %a" (Printexc.to_string e) Fmt.lines bt

exception UseAfterDropOrMove

(* Next we have dropable owned values.
 * These are resources together with all the runtime tracking information
 * needed to ensure that they are droped once. *)
module Owned = struct
  module Loc = struct
    type t = string

    let pp kind ppf t =
      Format.fprintf ppf "%s at %s" kind t
  end
  module T = struct
    type 'a t =
      { raw: 'a
      ; pp: 'a Fmt.t
      ; allocated_at: Loc.t
      ; last_moved_at: Loc.t option
      ; owner: Owner.t
      ; drop: 'a -> unit
      }

  (* Pretty printing *)
  let pp ppf t =
    Format.pp_open_vbox ppf 1;
    Fmt.(quote t.pp) ppf t.raw;
    Fmt.cut ppf ();
    Loc.pp "allocated" ppf t.allocated_at;
    Fmt.cut ppf ();
    Fmt.option (Loc.pp "last moved") ppf t.last_moved_at;
    Fmt.cut ppf ();
    Fmt.pf ppf "owned by %a" Owner.pp t.owner;
    Format.pp_close_box ppf ()

  (* safely destruct the value, at most once *)
  let drop t =
    try t.drop t.raw
    with e ->
      (* Drop has failed, at this point it may have leaked or not.
       * But it is not safe to call it again *)
      let open Xapi_stdext_pervasives.Pervasiveext in
      ignore_exn (fun () ->
        Backtrace.is_important e;
        Logs.err ~src (fun m -> m "Drop for %a raised: %a" pp t pp_exception e))
  end

  type 'a t = 'a T.t option ref

  let destructor t =
    match !t with
    | None -> () (* not an error *)
    | Some v ->
      (* prevent multiple drops and race conditions *)
      t := None;
      v.T.drop v.T.raw

  (* allocate a value and keep track of it *)
  let allocate ?(loc="") ~drop ~pp ~owner f =
    let raw = f () in
    let t = { T.raw; pp; allocated_at = loc; last_moved_at = None; owner; drop } in
    let r = ref (Some t) in
    Owner.add owner (fun () -> destructor r);
    r

  let drop = destructor

  let borrow t = match !t with
    | None -> raise UseAfterDropOrMove
    | Some t -> t

  let move ?loc t =
    let r = { (borrow t) with T.last_moved_at = loc } in
    t := None;
    ref (Some r)

  let borrow t = (borrow t).T.raw
end

(* Next we have a value with an associated destructor.  *)
module Destructible = struct
  type +'a t = { data: 'a; destructor: unit -> unit }

  let create ~drop ~pp data =
    let destructor () =
      try drop data
      with e ->
        Backtrace.is_important e;
        let open Xapi_stdext_pervasives.Pervasiveext in
        ignore_exn (fun () ->
            Logs.err ~src (fun m -> m "Drop for %a raised: %a" pp data pp_exn e))
    in
    { data ; destructor  }
end


(* We do not want to call a destructor more than once,
 * and we also do not want to keep the value alive after a destructor has been called.
 * We cannot track this using the type system (it would require support for affine types),
 * but we can track it at runtime.
 * *)
exception UseAfterMove

module Moveable : sig
  (** A type that can be moved, borrowed and dropped *)
  type 'a t

  (** [create raw] wraps [raw] with safe move semantics *)
  val create: 'a Destructible.t -> 'a t

  (** [borrow_exn t] gives access to the underlying raw value.
   * The lifetime of this cannot be tracked at compile- or run-time.
   * It is the caller's responsibility to ensure that returned ['a] value
   * is not used beyond the lifetime of its parent ['a t] value.
   * Typically the borrowed value should be used just for immediately passing it
   * on to a low-level function, such as those in the Unix module.
   *
   * Calling borrow on a value that has been moved is an error.
   * *)
  val borrow_exn: 'a t -> 'a

  (** [move_exn t] returns a freshly created value with same contents as [t],
   * and marks [t] as empty.
   * Move can only be called once on a value, it returns an error otherwise.
   * This may seem useless, however combined with lifetimes below becomes a useful way of
   * keeping track who owns a value.
   * It is also useful for executing a piece of code at most once, such as a destructor.
   * *)
  val move_exn: 'a t -> 'a t

  (** [drop t] *)
  val drop: 'a t -> unit
end = struct
  (* We could've used a bool here to track whether we called this yet or not,
   * but that'd unnecessarily keep the ['a] value live.
   * An option allows releasing ['a] after moving.
   * *)
  type 'a t = 'a Destructible.t option ref

  let create v = ref (Some v)

  let borrow_exn t = match !t with
    | None -> raise UseAfterMove
    | Some raw -> raw.Destructible.data

  let move_exn t =
    match !t with
    | None -> raise UseAfterMove
    | Some raw ->
    (* make sure this cannot be called again,
     * to avoid race conditions it is better to do this
     * before allocating more
     * *)
    t := None;
    create raw

  let drop t =
    match !t with
    | None -> () (* already moved/dropped *)
    | Some v ->
      t := None;
      v.Destructible.destructor ()
end

module StackOwner : sig
  include Dropable

  val create : unit -> t

  val add : t -> 'a Moveable.t -> unit
end = struct
  type t = (unit -> unit) Stack.t

  let create = Stack.create

  let add t (m:'a Moveable.t) =
    Stack.push (fun () -> Moveable.drop m) t

  let exec f = f ()

  let drop t =
    Stack.iter exec t
end

module Owner = struct
  type 'a t =
    { v: 'a
    ; owned: Destructor.t Stack.t Once.t
    }

  let create v = { v; owned = Once.create (Stack.create ()) }

  let add t (d: Destructor.t) =
    Stack.push d (Once.borrow_exn t)

  let drop t =
    Stack.iter Destructor.drop (Once.once_exn t.owned);
end


exception UseAfterMove


module SafeDropable(D: Dropable) = struct
  type t = { mutable dropable: D.t option
           ; allocated_at: string option
           ; name: string option }

  type raw = D.t

  let pp ppf t =
    Format.fprintf ppf "(Resource ";
    begin match t.name with
      | None -> ()
      | Some n -> Format.pp_print_string ppf n
    end;
    begin match t.allocated_at with
      | None -> ()
      | Some loc -> Format.fprintf ppf "allocated at %s" loc
    end;
    Format.fprintf ppf ")"

  let release t d =
    (* Mark this as freed first.
     * If drop raises it is not safe to call it again, might result in undefined behaviour.
     * *)
    t.dropable <- None;
    try D.drop d
    with e ->
      Backtrace.is_important e;
      let open Xapi_stdext_pervasives.Pervasiveext in
      ignore_exn (fun () ->
          Logs.err ~src (fun m -> m "Drop for %a raised: %a" pp t pp_exn e))

  let finaliser t =
    match t.dropable with
    | None -> ()
    | Some leaked ->
      let open Xapi_stdext_pervasives.Pervasiveext in
      (* do not raise inside a finaliser *)
      ignore_exn (fun () ->
          Logs.err ~src (fun m -> m "GC detected leaked resource %a" pp t);
          (* Lets heal the leak. This keeps a long running daemon alive.
           * The bug should be fixed, because there is no guarantee the finaliser runs soon enough!
           * *)
          release t leaked)

  let allocate ?loc ?name f =
    let t = { dropable = Some (f ()); allocated_at = loc; name } in
    Gc.finalise finaliser t;
    t

  let borrow t = match t.dropable with
    | None ->
      Logs.err ~src (fun m -> m "Use after release for resource %a" pp t);
      raise UseAfterRelease
    | Some d -> d

  let release_exn t =
    release t (borrow t)

  let drop t =
    match t.dropable with
    | None -> () (* idempotent *)
    | Some d ->
      release t d
end
   *)
