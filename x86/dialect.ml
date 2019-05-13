(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor (parts (c) 2010-2018 Institut National
   de Recherche en Informatique et en Automatique, Jade Alglave, and Luc
   Maranget)

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
   USE OR OTHER DEALINGS IN THE SOFTWARE.

   This file derives from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(****************************************************************************)
(* the diy toolsuite *)
(*  *)
(* Jade Alglave, University College London, UK. *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France. *)
(*  *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved. *)
(*  *)
(* This software is governed by the CeCILL-B license under French law and *)
(* abiding by the rules of distribution of free software. You can use, *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt. *)
(****************************************************************************)

open Base
open Utils
open Act_common
include Dialect_intf

module Att = struct
  let dialect : Id.t = Id.of_string "att"

  include Src_dst.Make (struct
    let operand_order = Src_dst.Src_then_dst
  end)

  let has_size_suffix = true

  let symbolic_jump_type = `Indirect
end

module Intel = struct
  let dialect : Id.t = Id.of_string "intel"

  include Src_dst.Make (struct
    let operand_order = Src_dst.Dst_then_src
  end)

  let has_size_suffix = false

  let symbolic_jump_type = `Immediate
end

module Herd7 = struct
  let dialect : Id.t = Id.of_string "herd7"

  include Src_dst.Make (struct
    let operand_order = Src_dst.Dst_then_src
  end)

  (* Surprisingly, this is true---for some operations, anyway. *)
  let has_size_suffix = true

  let symbolic_jump_type = `Immediate
end

let find_by_id (type a) (table : (Id.t, a) List.Assoc.t Lazy.t)
    ~(context:string)
  : (Id.t -> a Or_error.t) Staged.t =
  Staged.stage (fun id ->
  id
  |> List.Assoc.find (Lazy.force table) ~equal:[%equal: Id.t]
  |> Result.of_option
    ~error:(Error.create_s
              [%message
                "Unknown or unsupported x86 dialect"
                  ~context
                  ~id:(id : Id.t)
              ]
      ))
