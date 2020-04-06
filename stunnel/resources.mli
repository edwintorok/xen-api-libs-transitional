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

  val with_local: ((module S) -> 'a) -> 'a
end

exception UseAfterMove
module type Resource = sig
  type raw
  type 'scope t

  val with_borrow: (raw -> 'a) -> _ t -> 'a

  val move : into:'b Scope.T.t -> 'a t -> 'b t

  val free: _ t -> unit
end

module FD: Resource with type raw = Unix.file_descr

module Unix: sig
  val pipe: scope:'a Scope.T.t -> 'a FD.t * 'a FD.t
  val socketpair: scope:'a Scope.T.t -> Unix.socket_domain -> Unix.socket_type -> int -> 'a FD.t * 'a FD.t
  val openfile: scope:'a Scope.T.t -> string -> Unix.open_flag list -> int -> 'a FD.t
end
