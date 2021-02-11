(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t =
  { params: int Map.M(Common.Id).t [@default Map.empty (module Common.Id)]
  ; flags: Flag.t Map.M(Common.Id).t [@default Map.empty (module Common.Id)]
  }
[@@deriving sexp, make]

let find (m : 'a Map.M(Common.Id).t) ~(map_name : string) ~(id : Common.Id.t)
    : 'a Or_error.t =
  C4f_utils.My_map.find_or_error ~sexp_of_key:[%sexp_of: Common.Id.t]
    ~map_name m id

let get_param ({params; _} : t) ~(id : Common.Id.t) : int Or_error.t =
  find params ~id ~map_name:"parameter map"

let get_flag ({flags; _} : t) ~(id : Common.Id.t) : Flag.t Or_error.t =
  find flags ~id ~map_name:"flag map"

module Value = struct
  (* TODO(@MattWindsor91): consider representing in-map using this? *)

  type t = Param of int | Flag of Flag.t [@@deriving equal]

  let pp (f : Formatter.t) : t -> unit = function
    | Param k -> Fmt.(styled (`Fg `Blue) int) f k
    | Flag x ->
        ( match Flag.to_exact_opt x with
        | Some true -> Fmt.(styled (`Fg `Green) (any "on"))
        | Some false -> Fmt.(styled (`Fg `Red) (any "off"))
        | None ->
            Fmt.(
              styled
                (`Fg `Yellow)
                (concat ~sep:(any ":")
                   [using Flag.wins int; using Flag.losses int] )) )
          f x
end

let get (x : t) ~(id : Common.Id.t) : Value.t Or_error.t =
  Or_error.(
    find_ok
      [ (get_flag x ~id >>| fun x -> Value.Flag x)
      ; (get_param x ~id >>| fun x -> Value.Param x) ])

let rec calc_action_cap (lower : int) (upper : int) (flag : Flag.t)
    ~(random : Splittable_random.State.t) : int =
  if upper <= lower || not (Flag.eval flag ~random) then lower
  else calc_action_cap (lower + 1) upper flag ~random

let get_action_cap (x : t) ~(random : Splittable_random.State.t) :
    int Or_error.t =
  Or_error.Let_syntax.(
    let%bind flag = get_flag x ~id:Config_tables.extra_action_flag in
    let%bind lower = get_param x ~id:Config_tables.action_cap_lower_param in
    let%bind upper = get_param x ~id:Config_tables.action_cap_upper_param in
    if upper < lower then
      Or_error.error_s
        [%message
          "Upper action cap is below lower action cap"
            ~upper:(upper : int)
            ~lower:(lower : int)]
    else
      Ok
        ( match Flag.to_exact_opt flag with
        | Some true -> upper (* Always pick every extra action *)
        | Some false -> lower (* Never pick any extra action *)
        | None -> calc_action_cap lower upper flag ~random ))
