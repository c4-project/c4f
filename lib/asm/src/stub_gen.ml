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

  type config = Config.t

  let run_stub_gen (_osrc : Act_utils.Io.Out_sink.t) (_outp : _)
      ~(in_name : string) ~(program : Lang.Program.t)
      ~(symbols : Lang.Symbol.t list) ~(config : config)
      ~(passes : Act_config.Sanitiser_pass.Set.t) : Job.Output.t Or_error.t
      =
    ignore in_name ;
    ignore program ;
    ignore symbols ;
    ignore config ;
    ignore passes ;
    Or_error.unimplemented "stub_gen"

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
