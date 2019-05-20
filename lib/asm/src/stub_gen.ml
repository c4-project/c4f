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
include Stub_gen_intf

module Config = struct
  type t = unit

  (* For future expansion. *)
  let make () = ()

  let default = make ()
end

module Make (B : Basic) :
  S with module Lang = B.Src_lang and type config = Config.t = struct
  module Lang = B.Src_lang
  module San = Act_sanitiser.Instance.Make_single (B.Sanitiser_hook)

  type config = Config.t

  let gen_and_dump_asm_stub (listing : Lang.Program.t)
      (oc : Stdio.Out_channel.t) : unit Or_error.t =
    Or_error.Let_syntax.(
      let%map stub = B.as_asm_stub listing in
      let f = Caml.Format.formatter_of_out_channel oc in
      Act_c.Asm_stub.pp f stub)

  let run_stub_gen (_osrc : Act_utils.Io.Out_sink.t)
      (oc : Stdio.Out_channel.t) ~(in_name : string)
      ~(program : Lang.Program.t) ~(symbols : Lang.Symbol.t list)
      ~(config : config) ~(passes : Act_config.Sanitiser_pass.Set.t) :
      Job.Output.t Or_error.t =
    ignore config ;
    Or_error.Let_syntax.(
      let%bind san = San.sanitise ~passes ~symbols program in
      let program = San.Output.programs san in
      let listing = San.Output.Program.listing program in
      let%map () = gen_and_dump_asm_stub listing oc in
      let redirects_raw = San.Output.redirects san in
      let redirects = Lang.Symbol.R_map.to_string_alist redirects_raw in
      Job.Output.make (Fmt.always "?") in_name redirects [])

  module Filter : Runner.S with type cfg = Config.t = Runner.Make (struct
    type cfg = Config.t

    module Symbol = B.Src_lang.Symbol
    module Program = B.Program

    let name = "Stub generator"

    let tmp_file_ext = "txt"

    let default_config () = Config.default

    let run = run_stub_gen
  end)
end
