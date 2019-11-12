(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

type t = {o: Ac.Output.t [@default Ac.Output.silent ()]; vars: Var.Map.t}
[@@deriving fields, make]

let of_litmus ?(o : Ac.Output.t option) (lt : Act_c_mini.Litmus.Test.t) :
    t Or_error.t =
  Or_error.Let_syntax.(
    let%map vars = Var.Map.make_existing_var_map lt in
    make ?o ~vars ())

let try_map_vars (s : t) ~(f : Var.Map.t -> Var.Map.t Or_error.t) :
    t Or_error.t =
  Or_error.(s.vars |> f >>| fun vars -> {s with vars})

let map_vars (s : t) ~(f : Var.Map.t -> Var.Map.t) : t =
  {s with vars= f s.vars}

let register_global ?(initial_value : Act_c_mini.Constant.t option) (s : t)
    (var : Ac.C_id.t) (ty : Act_c_mini.Type.t) : t =
  map_vars s ~f:(fun v -> Var.Map.register_global v ?initial_value var ty)

let add_dependency (s : t) ~(id : Ac.Litmus_id.t) : t =
  map_vars s ~f:(Var.Map.add_dependency ~id)

let add_write (s : t) ~(id : Ac.Litmus_id.t) : t =
  map_vars s ~f:(Var.Map.add_write ~id)

let erase_var_value (s : t) ~(id : Ac.Litmus_id.t) : t Or_error.t =
  try_map_vars s ~f:(Var.Map.erase_value ~id)

let vars_satisfying_all (s : t) ~(scope : Ac.Scope.t)
    ~(predicates : (Var.Record.t -> bool) list) : Ac.C_id.t list =
  Var.Map.satisfying_all s.vars ~scope ~predicates

module Monad = struct
  module M = Travesty.State_transform.Make (struct
    module Inner = Or_error

    type nonrec t = t
  end)

  include M

  let with_vars_m (f : Var.Map.t -> 'a t) : 'a t = peek vars >>= f

  let with_vars (f : Var.Map.t -> 'a) : 'a t = peek vars >>| f

  let resolve (id : Ac.C_id.t) ~(scope : Ac.Scope.t) : Ac.Litmus_id.t t =
    with_vars (Ac.Scoped_map.resolve ~id ~scope)

  let register_global ?(initial_value : Act_c_mini.Constant.t option)
      (ty : Act_c_mini.Type.t) (var : Ac.C_id.t) : unit t =
    modify (fun s -> register_global ?initial_value s var ty)

  let add_dependency (id : Ac.Litmus_id.t) : unit t =
    modify (add_dependency ~id)

  module Exp_idents = Act_c_mini.Expression.On_identifiers.On_monad (M)

  let add_expression_dependencies (expr : Act_c_mini.Expression.t)
      ~(scope : Ac.Scope.t) : unit t =
    Exp_idents.iter_m expr ~f:(fun c_id ->
        c_id |> resolve ~scope >>= add_dependency)

  let add_write (id : Ac.Litmus_id.t) : unit t = modify (add_write ~id)

  let erase_var_value (id : Ac.Litmus_id.t) : unit t =
    Monadic.modify (erase_var_value ~id)

  let output () : Ac.Output.t t = peek (fun x -> x.o)
end
