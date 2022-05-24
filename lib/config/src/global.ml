(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* We can't use 'open struct' here, because the sedlex PPX only supports the
   4.07 AST. Aargh! *)

module Ac = C4f_common
module Au = C4f_utils
module Tx = Travesty_base_exts

type t =
  { fuzz: C4f_fuzz_run.Config.t
        [@sexp.option] [@default C4f_fuzz_run.Config.make ()] }
[@@deriving make, fields]

module Load : Plumbing.Loadable_types.S with type t = t = struct
  module File = struct
    let to_weight_opt : Ast.Fuzz.t -> (C4f_common.Id.t * int) option =
      function
      | Ast.Fuzz.Action (a, w) -> Some (a, Option.value w ~default:1)
      | Set _ -> None

    let to_param_opt : Ast.Fuzz.t -> (C4f_common.Id.t * int) option =
      function
      | Ast.Fuzz.Set (Param (k, v)) -> Some (k, v)
      | Set _ | Action _ -> None

    let interpret_flag : Ast.Fuzz.Flag_value.t -> C4f_fuzz.Flag.t Or_error.t
        = function
      | Exact b -> Or_error.return (C4f_fuzz.Flag.exact b)
      | Ratio (wins, losses) -> C4f_fuzz.Flag.try_make ~wins ~losses

    let to_flag_opt :
        Ast.Fuzz.t -> (C4f_common.Id.t * C4f_fuzz.Flag.t) Or_error.t option =
      function
      | Ast.Fuzz.Set (Flag (k, f)) ->
          Some (Or_error.map ~f:(fun v -> (k, v)) (interpret_flag f))
      | Set _ | Action _ -> None

    let fuzz_of_ast (ast : Ast.Fuzz.t list) :
        C4f_fuzz_run.Config.t Or_error.t =
      let weight_alist = List.filter_map ast ~f:to_weight_opt in
      let param_alist = List.filter_map ast ~f:to_param_opt in
      Or_error.Let_syntax.(
        let%bind flag_alist =
          Or_error.combine_errors (List.filter_map ast ~f:to_flag_opt)
        in
        let%bind weights =
          Map.of_alist_or_error (module C4f_common.Id) weight_alist
        in
        let%bind params =
          Map.of_alist_or_error (module C4f_common.Id) param_alist
        in
        let%map flags =
          Map.of_alist_or_error (module C4f_common.Id) flag_alist
        in
        C4f_fuzz_run.Config.make ~weights ~flags ~params ())

    let build_fuzz : Ast.t -> C4f_fuzz_run.Config.t option Or_error.t =
      Tx.Or_error.(
        Au.My_list.find_one_opt ~item_name:"fuzz" ~f:Ast.Top.as_fuzz
        >=> Tx.Option.With_errors.map_m ~f:fuzz_of_ast)

    let f (items : Ast.t) : t Or_error.t =
      Or_error.Let_syntax.(
        let%map fuzz = build_fuzz items in
        make ?fuzz ())

    type dst = t
  end

  include Plumbing.Loadable.Make_chain (Frontend) (File)
end
