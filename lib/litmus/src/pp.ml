(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Ac = Act_common
open Pp_intf

module Generic = struct
  let pp_location_stanza : Ac.C_id.t list option Fmt.t =
    Fmt.(
      option
        (hbox
           (any "locations@ " ++ brackets (box (list ~sep:semi Ac.C_id.pp)))))
end

module type Basic = sig
  module Test : Test_types.S

  val print_programs_inner :
    Out_channel.t -> Test.Lang.Program.t list -> unit
end

(** Makes the bits of a litmus AST that are common to all styles. *)
module Make_common (B : Basic) = struct
  let print_programs (oc : Out_channel.t) (ast : B.Test.t) : unit =
    B.print_programs_inner oc (B.Test.threads ast)

  let pp_init : (Ac.C_id.t, B.Test.Lang.Constant.t) List.Assoc.t Fmt.t =
    Act_utils.My_format.pp_c_braces
      Fmt.(
        list ~sep:sp (fun f (l, c) ->
            pf f "@[%a = %a;@]" Ac.C_id.pp l B.Test.Lang.Constant.pp c))

  let pp_post : B.Test.Lang.Constant.t Postcondition.t Fmt.t =
    Postcondition.pp ~pp_const:B.Test.Lang.Constant.pp

  let print_body (oc : Out_channel.t) (litmus : B.Test.t) : unit =
    let f = Caml.Format.formatter_of_out_channel oc in
    pp_init f (B.Test.init litmus) ;
    Fmt.pf f "@.@." ;
    print_programs oc litmus ;
    Fmt.pf f "@." ;
    Generic.pp_location_stanza f (B.Test.locations litmus) ;
    Fmt.(option (any "@,@," ++ pp_post)) f (B.Test.postcondition litmus) ;
    Fmt.flush f ()

  let print (oc : Out_channel.t) (litmus : B.Test.t) : unit =
    let lang_name = B.Test.Lang.name in
    let test_name = B.Test.name litmus in
    Out_channel.fprintf oc "%s %s\n\n%a" lang_name test_name print_body
      litmus
end

module Make_sequential (Test : Test_types.S) : S with module Test = Test =
struct
  module Test = Test

  module Specific = struct
    let print_programs_inner (oc : Stdio.Out_channel.t) :
        Test.Lang.Program.t list -> unit =
      Fmt.(vbox (list ~sep:(cut ++ cut) Test.Lang.Program.pp) ++ flush)
        (Caml.Format.formatter_of_out_channel oc)
  end

  include Make_common (struct
    module Test = Test
    include Specific
  end)
end
