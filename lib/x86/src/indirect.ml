(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique et en
   Automatique, Jade Alglave, and Luc Maranget)

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

   This file derives in part from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(* the diy toolsuite

   Jade Alglave, University College London, UK.

   Luc Maranget, INRIA Paris-Rocquencourt, France.

   Copyright 2010-present Institut National de Recherche en Informatique et
   en Automatique and the authors. All rights reserved.

   This software is governed by the CeCILL-B license under French law and by
   the rules of distribution of free software. You can use, and/ or
   redistribute the software under the terms of the CeCILL-B license as
   circulated by CEA, CNRS and INRIA at the following URL
   "http://www.cecill.info". We also give a copy in LICENSE.txt. *)

(** x86 AST: indirect memory accesses. *)

open Base
open Base_quickcheck
module Tx = Travesty_base_exts

type t =
  { seg: Reg.t option
  ; disp: Disp.t option
  ; base: Reg.t option
  ; index: Index.t option }
[@@deriving sexp, equal, compare, fields, make, quickcheck]

(** Base mapper for memory addresses *)
module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let map_m indirect ~seg ~disp ~base ~index =
    Fields.fold ~init:(M.return indirect) ~seg:(F.proc_field seg)
      ~disp:(F.proc_field disp) ~base:(F.proc_field base)
      ~index:(F.proc_field index)
end

(** Recursive mapper for symbols *)
module On_symbols :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = string =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = String

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module D = Disp.On_symbols.On_monad (M)
    module O = Tx.Option.On_monad (M)

    let map_m t ~f =
      B.map_m t
        ~disp:
          (O.map_m ~f:(D.map_m ~f))
          (* Segments, bases, and indices have no symbols. *)
        ~seg:M.return ~base:M.return ~index:M.return
  end
end)

(** Recursive mapper for registers *)
module On_registers :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Reg.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Reg

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module O = Tx.Option.On_monad (M)
    module I = Index.On_registers.On_monad (M)

    let map_m t ~f =
      B.map_m t ~seg:(O.map_m ~f) ~base:(O.map_m ~f)
        ~index:
          (O.map_m ~f:(I.map_m ~f)) (* Displacements have no registers. *)
        ~disp:M.return
  end
end)

let as_disp_only : t -> Disp.t option = function
  | {disp= Some d; base= None; seg= None; index= None} ->
      Some d
  | _ ->
      None

let as_symbolic_disp_only : t -> string option =
  Tx.Option.compose_m as_disp_only Disp.as_symbol
