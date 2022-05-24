(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t =
  { (* Optionals to the top, to make sure [make] derives correctly. *)
    o: Common.Output.t [@default Common.Output.silent ()]
  ; labels: Set.M(Common.Litmus_id).t
        [@default Set.empty (module Common.Litmus_id)]
  ; vars: Var.Map.t }
[@@deriving accessors, make]

let of_litmus ?(o : Common.Output.t option) (lt : Fir.Litmus.Test.t) :
    t Or_error.t =
  let labels = Label.labels_of_test lt in
  Or_error.Let_syntax.(
    let%map vars = Var.Map.make_existing_var_map lt in
    make ?o ~vars ~labels ())

let register_label (s : t) ~(label : Common.Litmus_id.t) : t =
  {s with labels= Set.add s.labels label}

let register_var (s : t) (var : Common.Litmus_id.t)
    (init : C4f_fir.Initialiser.t) : t =
  let ty = Accessor.get C4f_fir.Initialiser.ty init in
  let initial_value = Accessor.get C4f_fir.Initialiser.value init in
  Accessor.map vars s ~f:(fun v ->
      Var.Map.register_var v ~initial_value var ty )

let add_dependency (s : t) ~(id : Common.Litmus_id.t) : t =
  Accessor.map vars s ~f:(Var.Map.add_dependency ~id)

let add_write (s : t) ~(id : Common.Litmus_id.t) : t =
  Accessor.map vars s ~f:(Var.Map.add_write ~id)

let erase_var_value (s : t) ~(id : Common.Litmus_id.t) : t Or_error.t =
  Utils.Accessor.On_error.map vars s ~f:(Var.Map.erase_value ~id)

module Monad = struct
  module M = Travesty.State_transform.Make (struct
    module Inner = Or_error

    type nonrec t = t
  end)

  include M

  let peek_acc acc = peek (Accessor.get acc)

  let with_vars_m (f : Var.Map.t -> 'a t) : 'a t = peek_acc vars >>= f

  let with_vars (f : Var.Map.t -> 'a) : 'a t = peek_acc vars >>| f

  let with_labels_m (f : Set.M(Common.Litmus_id).t -> 'a t) : 'a t =
    peek_acc labels >>= f

  let with_labels (f : Set.M(Common.Litmus_id).t -> 'a) : 'a t =
    peek_acc labels >>| f

  let resolve (id : Common.C_id.t) ~(scope : Common.Scope.t) :
      Common.Litmus_id.t t =
    with_vars (Common.Scoped_map.resolve ~id ~scope)

  let register_var (var : Common.Litmus_id.t) (init : C4f_fir.Initialiser.t)
      : unit t =
    modify (fun s -> register_var s var init)

  let register_and_declare_var (var : Common.Litmus_id.t)
      (init : C4f_fir.Initialiser.t) (subject : Subject.Test.t) :
      Subject.Test.t t =
    register_var var init
    >>= fun () -> Monadic.return (Subject.Test.declare_var subject var init)

  let register_label (label : Common.Litmus_id.t) : unit t =
    modify (register_label ~label)

  let add_dependency (id : Common.Litmus_id.t) : unit t =
    modify (add_dependency ~id)

  module AccM = Accessor.Of_monad (struct
    include M

    let apply = `Define_using_bind
  end)

  let add_scoped_dependency (id : Common.C_id.t) ~(scope : Common.Scope.t) :
      unit t =
    id |> resolve ~scope >>= add_dependency

  let add_expression_dependencies (expr : C4f_fir.Expression.t)
      ~(scope : Common.Scope.t) : unit t =
    AccM.iter C4f_fir.Expression_traverse.depended_upon_idents expr
      ~f:(add_scoped_dependency ~scope)

  let add_multiple_expression_dependencies
      (exprs : C4f_fir.Expression.t list) ~(scope : Common.Scope.t) : unit t
      =
    AccM.iter
      Accessor_base.(
        List.each @> C4f_fir.Expression_traverse.depended_upon_idents)
      exprs
      ~f:(add_scoped_dependency ~scope)

  let add_expression_dependencies_at_path (exprs : C4f_fir.Expression.t list)
      ~(path : Path.With_meta.t) : unit t =
    let in_dead_code = path.@(Path.With_meta.flag In_dead_code) in
    let tid = Path.tid path.path in
    unless_m in_dead_code ~f:(fun () ->
        add_multiple_expression_dependencies exprs ~scope:(Local tid) )

  let add_write (id : Common.Litmus_id.t) : unit t = modify (add_write ~id)

  let erase_var_value (id : Common.Litmus_id.t) : unit t =
    Monadic.modify (erase_var_value ~id)

  let output () : Common.Output.t t = peek (fun x -> x.o)

  module Acc = Accessor.Of_monad (struct
    include M

    let apply = `Define_using_bind
  end)
end

let pp_labels : Set.M(Common.Litmus_id).t Fmt.t =
  Utils.My_format.pp_set Common.Litmus_id.pp

let pp_map : Var.Map.t Fmt.t = Common.Scoped_map.pp Var.Record.pp

let pp : t Fmt.t =
  Fmt.(
    concat
      [ vbox ~indent:2
          (any "Labels" ++ sp ++ using (Accessor.get labels) pp_labels)
      ; vbox ~indent:2 (any "Vars" ++ sp ++ using (Accessor.get vars) pp_map)
      ])

module Dump : Plumbing.Storable_types.S with type t = t =
Plumbing.Storable.Make (struct
  type nonrec t = t

  let store_to_oc ?path:(_ : string option) (state : t)
      ~(dest : Stdio.Out_channel.t) : unit Or_error.t =
    Ok (Utils.My_format.fdump dest (Fmt.vbox pp) state)
end)
