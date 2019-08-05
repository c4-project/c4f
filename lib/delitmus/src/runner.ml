(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

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
end

let driver_of_style : Style.t -> (module Driver.S) = function
  | Vars_as_globals ->
      (module Driver.Vars_as_globals)
  | Vars_as_parameters ->
      (module Driver.Vars_as_parameters)

let run (input : Act_c_mini.Litmus.Test.t) ~(style : Style.t) :
    Output.t Or_error.t =
  let (module M) = driver_of_style style in
  M.run input
