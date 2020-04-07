module type Closeable = sig
  type t
  val close : t -> unit
end

val on_close_exception_hook: (exn -> unit) ref

module Scope: sig
  module T : sig
    type 'a t
  end
  module type S = sig
    type t
    val scope: t T.t
  end
  type any
  val any: 'a T.t -> any T.t

  val with_local: ((module S) -> 'a) -> 'a
end

exception UseAfterMove
module type Resource = sig
  type raw
  type 'scope t

  val borrow: _ t -> raw

  val move : into:'b Scope.T.t -> 'a t -> 'b t

  val free: _ t -> unit
end

module Make(R: Closeable) : Resource with type raw = R.t

module FD: Resource with type raw = Unix.file_descr

module Unix: sig
  val pipe: scope:'a Scope.T.t -> 'a FD.t * 'a FD.t
  val socketpair: scope:'a Scope.T.t -> Unix.socket_domain -> Unix.socket_type -> int -> 'a FD.t * 'a FD.t
  val openfile: scope:'a Scope.T.t -> string -> Unix.open_flag list -> int -> 'a FD.t
end

module Table(R: Resource): sig
  type scope
  type 'a t

  val create : int -> 'a t
  val add: 'a t -> 'a -> 'b R.t -> unit
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> 'a -> 'b R.t -> unit
  val find: 'a t -> 'a -> scope R.t
  val mem: 'a t -> 'a -> bool
  val remove: 'a t -> 'a -> unit
  val replace: 'a t -> 'a -> _ R.t -> unit
  val iter: ('a -> scope R.t -> unit) -> 'a t -> unit
  val fold: ('a -> scope R.t -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length: 'a t -> int
end
