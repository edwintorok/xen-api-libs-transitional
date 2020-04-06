(* who should implement recursive cleanup?
 * the local scope, or the "resource".
 * Following Rust's drop trait it should be the drop itself that gets called recursively
 * by another mechanism!
 *
 *
 * you can't move arbitrary thing into scopes of things, you have to use a supported way!
 * e.g. methods that do move!
 *
 * outer first, consider stunnel + fd
 * *)

module type Closeable = sig
  type t
  val close : t -> unit
end

(* TODO: override in resourcesdebug *)
let on_close_exception_hook = ref ignore

let exec_noerr f =
  try f ()
  with e ->
    try !on_close_exception_hook e
    with _ -> ()

let with_local f =
  let s = Stack.create () in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f (fun cleanup -> Stack.push cleanup s))
    (fun () -> Stack.iter exec_noerr s)

(*: sig
  module T : sig
    type 'a t
  end
  module type S = sig
    type t
    val scope: t T.t
  end

  val with_local: ((module S) -> 'a) -> 'a
end = *)
module Scope = struct
  module T = struct
    type 'a t = (unit -> unit) -> unit
  end
  module type S = sig
    type t
    val scope: t T.t
  end

  let with_local f =
    (* Generates a new unique type that can be used to prevent
     * accidental resource leaks (applies types taking L.scope as parameter): {|
       utop # Scope.with_local @@ fun (module L) -> L.scope;;
       Line 1, characters 38-45:
       Error: This expression has type L.t Scope.T.t
         but an expression was expected of type 'a
         The type constructor L.t would escape its scope

       It won't prevent leaking closures that access the value though.
       |} *)
    with_local (fun scope ->
        f (module struct type t let scope = scope end: S))
end

exception UseAfterMove
module type Resource = sig
  type raw
  type 'scope t

  val with_borrow: (raw -> 'a) -> _ t -> 'a

  val move : into:'b Scope.T.t -> 'a t -> 'b t

  val free: _ t -> unit
end

module Make(R: Closeable) = struct
  type raw = R.t
  type 'scope t = raw option ref

  let close_ifactive t =
    match !t with
    | None -> () (* already closed/moved *)
    | Some active -> R.close active

  let of_raw scope raw =
    let t = ref (Some raw) in
    scope (fun () -> close_ifactive t);
    t

  let allocate scope f = of_raw scope @@ f ()

  let allocate2 scope f =
    let r1, r2 = f () in
    of_raw scope r1, of_raw scope r2

  let with_borrow f t = match !t with
    | None -> raise UseAfterMove
    | Some r -> f r

  let move ~into t =
    let r = with_borrow (of_raw into) t in
    t := None;
    r

  let free t =
    (* freeing is equivalent to moving into a local scope
     * that is immediately closed *)
    Scope.with_local @@ fun (module L) ->
    move L.scope t |> ignore
end

module FD = Make(struct
    type t = Unix.file_descr
    let close = Unix.close
end)

module Unix = struct
  let pipe ~scope = FD.allocate2 scope Unix.pipe
  let socketpair ~scope domain typ protocol =
    FD.allocate2 scope (fun () -> Unix.socketpair domain typ protocol)

  let openfile ~scope path flags perm = FD.allocate scope (fun () -> Unix.openfile path flags perm)
end
(* TODO: thread local "scope" on the stack *)

(* TODO: data structure nesting in table *)
