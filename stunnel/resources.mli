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
   * *)
  val drop: t -> unit
end

exception UseAfterRelease

(** A type that protects against double free and use after free *)
module SafeDropable(D: Dropable): sig
  (** D.t wrapped *)
  type t

  type raw = D.t

  (** [allocate ?loc ?name f] wraps the resource allocated by [f]
   * with an optional location, name and leak tracking.
   * A GC finaliser is attached to help debug leaks, it is only a safeguard,
   * it should not be relied upon for proper leak avoidance!
   * Example usage: [allocate ~loc:__LOC__ ~name:"pipe for stunnel config"]
   * *)
  val allocate: ?loc:string -> ?name:string -> (unit -> D.t) -> t

  (** [borrow t] gives access to the underlying raw type.
   * Access to the returned value should not outlive [t] itself.
   * This cannot be checked by the type system or at runtime in general!
   * (file descriptors are typically integers, with no finalisers attached).
   * *)
  val borrow: t -> raw

  (** [release_exn resource] like [drop resource] but raises an exception
   * if already dropped *)
  val release_exn: t -> unit
end

