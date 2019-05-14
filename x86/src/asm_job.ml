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
open Act_common
module Tx = Travesty_base_exts

let get_runner (dialect : Id.t) =
  let open Or_error.Let_syntax in
  let%bind (module Frontend) = Frontend.of_dialect dialect in
  let%map (module Lang) = Language_definition.of_dialect dialect in
  ( module Asm.Runner.Make (struct
    type ast = Ast.t

    module Src_lang = Lang
    module Dst_lang = Language_definition.Herd7
    module Frontend = Frontend

    module Litmus_ast = Litmus.Ast.Make (struct
      module Program = struct
        include Dst_lang.Program

        let global_vars = Fn.const None
      end

      include (
        Dst_lang : module type of Dst_lang with module Program := Program )

      module Type = Unit
    end)

    module Litmus_pp = Litmus.Pp.Make_tabular (Litmus_ast)
    module Multi_sanitiser = Sanitiser.Make_multi (Src_lang)
    module Single_sanitiser = Sanitiser.Make_single (Src_lang)
    module Conv = Conv.Make (Src_lang) (Dst_lang)

    let convert_program = Conv.convert

    let convert_const = Or_error.return

    let program = Fn.id
  end)
  : Asm.Runner.S )
