(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
end

type t =
  { (* Optionals to the top, to make sure [make] derives correctly. *)
    o: Ac.Output.t [@default Ac.Output.silent ()]
  ; labels: Set.M(Ac.Litmus_id).t [@default Set.empty (module Ac.Litmus_id)]
  ; vars: Var.Map.t }
[@@deriving fields, make]

let of_litmus ?(o : Ac.Output.t option) (lt : Act_fir.Litmus.Test.t) :
    t Or_error.t =
  let labels = Label.labels_of_test lt in
  Or_error.Let_syntax.(
    let%map vars = Var.Map.make_existing_var_map lt in
    make ?o ~vars ~labels ())

let try_map_vars (s : t) ~(f : Var.Map.t -> Var.Map.t Or_error.t) :
    t Or_error.t =
  Or_error.(s.vars |> f >>| fun vars -> {s with vars})

let map_vars (s : t) ~(f : Var.Map.t -> Var.Map.t) : t =
  {s with vars= f s.vars}

let register_label (s : t) ~(label : Ac.Litmus_id.t) : t =
  {s with labels= Set.add s.labels label}

let register_var (s : t) (var : Ac.Litmus_id.t)
    (init : Act_fir.Initialiser.t) : t =
  let ty = Accessor.get Act_fir.Initialiser.ty init in
  let initial_value = Accessor.get Act_fir.Initialiser.value init in
  map_vars s ~f:(fun v -> Var.Map.register_var v ~initial_value var ty)

let add_dependency (s : t) ~(id : Ac.Litmus_id.t) : t =
  map_vars s ~f:(Var.Map.add_dependency ~id)

let add_write (s : t) ~(id : Ac.Litmus_id.t) : t =
  map_vars s ~f:(Var.Map.add_write ~id)

let erase_var_value (s : t) ~(id : Ac.Litmus_id.t) : t Or_error.t =
  try_map_vars s ~f:(Var.Map.erase_value ~id)

module Monad = struct
  module M = Travesty.State_transform.Make (struct
    module Inner = Or_error

    type nonrec t = t
  end)

  include M

  let with_vars_m (f : Var.Map.t -> 'a t) : 'a t = peek vars >>= f

  let with_vars (f : Var.Map.t -> 'a) : 'a t = peek vars >>| f

  let with_labels_m (f : Set.M(Ac.Litmus_id).t -> 'a t) : 'a t =
    peek labels >>= f

  let with_labels (f : Set.M(Ac.Litmus_id).t -> 'a) : 'a t =
    peek labels >>| f

  let resolve (id : Ac.C_id.t) ~(scope : Ac.Scope.t) : Ac.Litmus_id.t t =
    with_vars (Ac.Scoped_map.resolve ~id ~scope)

  let register_var (var : Ac.Litmus_id.t) (init : Act_fir.Initialiser.t) :
      unit t =
    modify (fun s -> register_var s var init)

  let register_and_declare_var (var : Ac.Litmus_id.t)
      (init : Act_fir.Initialiser.t) (subject : Subject.Test.t) :
      Subject.Test.t t =
    register_var var init
    >>= fun () -> Monadic.return (Subject.Test.declare_var subject var init)

  let register_label (label : Ac.Litmus_id.t) : unit t =
    modify (register_label ~label)

  let add_dependency (id : Ac.Litmus_id.t) : unit t =
    modify (add_dependency ~id)

  module AccM = Accessor.Of_monad (struct
    include M

    let apply = `Define_using_bind
  end)

  let add_scoped_dependency (id : Ac.C_id.t) ~(scope : Ac.Scope.t) : unit t =
    id |> resolve ~scope >>= add_dependency

  let add_expression_dependencies (expr : Act_fir.Expression.t)
      ~(scope : Ac.Scope.t) : unit t =
    AccM.iter Act_fir.Expression_traverse.depended_upon_idents expr
      ~f:(add_scoped_dependency ~scope)

  let add_multiple_expression_dependencies
      (exprs : Act_fir.Expression.t list) ~(scope : Ac.Scope.t) : unit t =
    AccM.iter
      Accessor_base.(
        List.each @> Act_fir.Expression_traverse.depended_upon_idents)
      exprs
      ~f:(add_scoped_dependency ~scope)

  let add_expression_dependencies_at_path (exprs : Act_fir.Expression.t list)
      ~(path : Path.Flagged.t) : unit t =
    let in_dead_code = Set.mem (Path_flag.Flagged.flags path) In_dead_code in
    let tid = Path.tid (Path_flag.Flagged.path path) in
    unless_m in_dead_code ~f:(fun () ->
        add_multiple_expression_dependencies exprs ~scope:(Local tid))

  let add_write (id : Ac.Litmus_id.t) : unit t = modify (add_write ~id)

  let erase_var_value (id : Ac.Litmus_id.t) : unit t =
    Monadic.modify (erase_var_value ~id)

  let output () : Ac.Output.t t = peek (fun x -> x.o)
end
