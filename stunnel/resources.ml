let src = Logs.Src.create "resources" ~doc:"logs from Xapi_stdext_pervasives.Resources"

module type Dropable = sig
  (** a resource *)
  type t

  (** [drop resource] releases [resource]. E.g. close a file, unlock a mutex, etc.
   * Calling this multiple times may result in undefined behaviour.
   * *)
  val drop: t -> unit
end

exception UseAfterRelease

let pp_exn ppf e =
  let bt = e |> Backtrace.get |> Backtrace.to_string_hum in
  Format.fprintf ppf "%s (%s)" (Printexc.to_string e) bt

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
