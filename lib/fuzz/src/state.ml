(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

type t = {vars: Var.Map.t; o: Ac.Output.t} [@@deriving fields]

let init ?(o : Ac.Output.t = Ac.Output.silent ())
    ~(globals : Act_c_mini.Type.t Ac.C_id.Map.t) ~(locals : Ac.C_id.Set.t)
    () : t =
  let vars = Var.Map.make_existing_var_map globals locals in
  {vars; o}

let try_map_vars (s : t) ~(f : Var.Map.t -> Var.Map.t Or_error.t) :
    t Or_error.t =
  Or_error.(s.vars |> f >>| fun vars -> {s with vars})

let map_vars (s : t) ~(f : Var.Map.t -> Var.Map.t) : t =
  {s with vars= f s.vars}

let register_global ?(initial_value : Act_c_mini.Constant.t option) (s : t)
    (var : Ac.C_id.t) (ty : Act_c_mini.Type.t) : t =
  map_vars s ~f:(fun v -> Var.Map.register_global v ?initial_value var ty)

let add_dependency (s : t) ~(var : Ac.C_id.t) : t =
  map_vars s ~f:(Var.Map.add_dependency ~var)

let add_write (s : t) ~(var : Ac.C_id.t) : t =
  map_vars s ~f:(Var.Map.add_write ~var)

let erase_var_value (s : t) ~(var : Ac.C_id.t) : t Or_error.t =
  try_map_vars s ~f:(Var.Map.erase_value ~var)

let vars_satisfying_all (s : t) ~(predicates : (Var.Record.t -> bool) list)
    : Ac.C_id.t list =
  Var.Map.satisfying_all s.vars ~predicates

module Monad = struct
  include Travesty.State_transform.Make (struct
    module Inner = Or_error

    type nonrec t = t
  end)

  let with_vars_m (f : Var.Map.t -> 'a t) : 'a t = peek vars >>= f

  let with_vars (f : Var.Map.t -> 'a) : 'a t = peek vars >>| f

  let register_global ?(initial_value : Act_c_mini.Constant.t option)
      (ty : Act_c_mini.Type.t) (var : Ac.C_id.t) : unit t =
    modify (fun s -> register_global ?initial_value s var ty)

  let add_dependency (var : Ac.C_id.t) : unit t =
    modify (add_dependency ~var)

  let add_write (var : Ac.C_id.t) : unit t = modify (add_write ~var)

  let erase_var_value (var : Ac.C_id.t) : unit t =
    Monadic.modify (erase_var_value ~var)

  let output () : Ac.Output.t t = peek (fun x -> x.o)
end
