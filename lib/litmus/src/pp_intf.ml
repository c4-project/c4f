(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Id = Act_common.Litmus_id

module type S = sig
  module Test : Test_types.S

  val print : Out_channel.t -> Test.t -> unit
  (** [print oc ast] prints [ast] on output channel [oc]. *)

  val print_programs : Out_channel.t -> Test.t -> unit
  (** [print_programs oc ast] prints the program table of [ast] on output
      channel [oc], omitting all of the other parts of the AST. *)

  val pp_post : Test.Lang.Constant.t Postcondition.t Fmt.t
  (** [pp_post f post] prints the postcondition [post] on formatter [f]. *)
end
