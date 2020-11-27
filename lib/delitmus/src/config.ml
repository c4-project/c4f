(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Style = struct
  module M = struct
    type t = Vars_as_globals | Vars_as_parameters
    [@@deriving compare, equal, enumerate]

    let table : (t, string) List.Assoc.t =
      [ (Vars_as_globals, "vars-as-globals")
      ; (Vars_as_parameters, "vars-as-parameters") ]
  end

  include M

  include Act_utils.Enum.Extend_table (struct
    include M
    include Act_utils.Enum.Make_from_enumerate (M)
  end)

  let to_rewriter : t -> (module Function_rewriter.S) = function
    | Vars_as_globals ->
        (module Function_rewriter.Vars_as_globals)
    | Vars_as_parameters ->
        (module Function_rewriter.Vars_as_parameters)

  (* So far, there is no distinction between global and local mapping. This
     might change. *)
  let to_mapping : t -> (int -> Var_map.Mapping.t) Staged.t = function
    | Vars_as_globals ->
        Staged.stage (Fn.const Var_map.Mapping.Global)
    | Vars_as_parameters ->
        Staged.stage (fun i -> Var_map.Mapping.Param i)
end

type t =
  { impl_suffix: string option
  ; qualify_locals: bool [@default true]
  ; style: Style.t }
[@@deriving fields, make]

let to_runner (config : t) : (module Runner_types.S) =
  let style = style config in
  let (module F) = Style.to_rewriter style in
  ( module Runner.Make (struct
    let global_mapping = Staged.unstage (Style.to_mapping style)

    let local_mapping = Staged.unstage (Style.to_mapping style)

    let impl_suffix = impl_suffix config

    let qualify_locals = qualify_locals config

    module Function = F
  end) )
