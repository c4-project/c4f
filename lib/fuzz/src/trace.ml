(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel (* for Fqueue *)

type elt = {name: Act_common.Id.t; payload: Sexp.t} [@@deriving sexp]

module M = struct
  type t = elt Fqueue.t [@@deriving sexp]
end

include M
include Plumbing.Loadable.Of_sexpable (M)
include Plumbing.Storable.Of_sexpable (M)

let empty : t = Fqueue.empty

let add (type p) (trace : t)
    ~(action : (module Action_types.S with type Payload.t = p))
    ~(payload : p) : t =
  let (module Action) = action in
  let name = Action.name in
  let payload = Action.Payload.sexp_of_t payload in
  Fqueue.enqueue trace {name; payload}

type step =
  | Stop
  | Action of {f: Subject.Test.t -> Subject.Test.t State.Monad.t; tail: t}
  | Error of Error.t

let resolve_step ({name; payload= payload_sexp} : elt)
    ~(resolve : Act_common.Id.t -> (module Action_types.S) Or_error.t) :
    (Subject.Test.t -> Subject.Test.t State.Monad.t) Or_error.t =
  Or_error.Let_syntax.(
    let%bind (module Action) = resolve name in
    let%map payload =
      Or_error.try_with (fun () -> Action.Payload.t_of_sexp payload_sexp)
    in
    Action.run ~payload)

let run_step (trace : t)
    ~(resolve : Act_common.Id.t -> (module Action_types.S) Or_error.t) : step
    =
  match Fqueue.dequeue trace with
  | None ->
      Stop
  | Some (elt, tail) -> (
    match resolve_step elt ~resolve with
    | Ok f ->
        Action {f; tail}
    | Error e ->
        Error e )

let run (trace : t) (test : Subject.Test.t)
    ~(resolve : Act_common.Id.t -> (module Action_types.S) Or_error.t) :
    Subject.Test.t State.Monad.t =
  State.Monad.Let_syntax.(
    let%map _, test' =
      State.Monad.fix (trace, test) ~f:(fun mu (this_trace, this_test) ->
          match run_step this_trace ~resolve with
          | Stop ->
              State.Monad.return (this_trace, this_test)
          | Error e ->
              State.Monad.Monadic.return (Error e)
          | Action {f; tail= next_trace} ->
              let%bind next_test = f this_test in
              mu (next_trace, next_test))
    in
    test')

let length : t -> int = Fqueue.length

let take (trace : t) (n : int) : t =
  let s = Fqueue.to_sequence trace in
  Fqueue.of_sequence (Sequence.take s n)

let bisect_index (trace : t) ~(want : [`Last_on_left | `First_on_right])
    ~(f : t -> [`Left | `Right]) : int option =
  (* Taking length+1 is an artefact of trying to use something that finds one
     element of a list to find a sublist of a list; it lets us consider all
     traces from empty up to full. *)
  Binary_search.binary_search_segmented trace want
    ~length:(Fn.compose succ length) ~get:take ~segment_of:f

let default_for (trace : t) ~(want : [`Last_on_left | `First_on_right]) : t =
  match want with `Last_on_left -> empty | `First_on_right -> trace

let bisect (trace : t) ~(want : [`Last_on_left | `First_on_right])
    ~(f : t -> [`Left | `Right]) : t =
  trace |> bisect_index ~f ~want
  |> Option.map ~f:(take trace)
  |> Option.value ~default:(default_for trace ~want)
