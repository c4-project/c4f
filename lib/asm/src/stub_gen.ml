(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
module Tx = Travesty_base_exts
open Stub_gen_intf

module Config = struct
  type t = {separator: string option} [@@deriving fields, make]

  let default = make ()
end

module type S = S with type config := Config.t

module type S_filter = Runner_intf.S with type cfg = Config.t

(* TODO(@MattWindsor91): don't print separator after the last element. *)
let print_separator (f : Formatter.t) (config : Config.t) : unit =
  Option.iter (Config.separator config) ~f:(Fmt.pf f "@[%s@]@.")

module Make (B : Basic) : S with module Lang = B.Src_lang = struct
  module Lang = B.Src_lang
  module San = Act_sanitiser.Instance.Make (B.Sanitiser_hook)

  let gen_and_dump_asm_stub (i : int) (program : Lang.Program.t)
      ~(oc : Stdio.Out_channel.t) ~(config : Config.t) : unit Or_error.t =
    let f = Caml.Format.formatter_of_out_channel oc in
    if Int.(i <> 0) then print_separator f config ;
    Or_error.Let_syntax.(
      let%map stub = B.as_asm_stub i program in
      Fmt.pf f "@[<v>%a@]@." Act_c.Asm_stub.pp stub)

  let gen_and_dump_asm_stubs (programs : Lang.Program.t list)
      ~(oc : Stdio.Out_channel.t) ~(config : Config.t) : unit Or_error.t =
    programs
    |> List.mapi ~f:(gen_and_dump_asm_stub ~oc ~config)
    |> Or_error.combine_errors_unit

  let run_stub_gen ?(c_variables : Act_common.C_variables.Map.t option)
      (oc : Stdio.Out_channel.t) ~(in_name : string)
      ~(programs : Lang.Program.t list)
      ~(config : Config.t) :
      unit Or_error.t =
    ignore config ;
    ignore c_variables ;
    ignore in_name ;
    gen_and_dump_asm_stubs programs ~oc ~config

  module Filter : S_filter = Runner.Make (struct
    type cfg = Config.t
    type aux_o = unit

    module Symbol = B.Src_lang.Symbol

    type program = B.Src_lang.Program.t
    module Litmus = B.Litmus

    let name = "Stub generator"

    let tmp_file_ext = "txt"

    let default_config () = Config.default

    let run = run_stub_gen
  end)
end
