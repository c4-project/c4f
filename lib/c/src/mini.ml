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

open Core_kernel
module Tx = Travesty_base_exts
module Ac = Act_common
module Address = Mini_address
module Assign = Mini_assign
module Atomic_cmpxchg = Mini_atomic_cmpxchg
module Atomic_load = Mini_atomic_load
module Atomic_store = Mini_atomic_store
module Constant = Act_c_lang.Ast_basic.Constant
module Env = Mini_env
module Expression = Mini_expression
module Function = Mini_function
module Identifier = Act_c_lang.Ast_basic.Identifier
module Initialiser = Mini_initialiser
module Lvalue = Mini_lvalue
module Pointer = Act_c_lang.Ast_basic.Pointer
module Statement = Mini_statement
module Type = Mini_type
open Mini_intf

module Program = struct
  type t = {globals: Initialiser.t id_assoc; functions: Function.t id_assoc}
  [@@deriving sexp, fields, make]

  let with_functions (program : t) (new_functions : Function.t id_assoc) : t
      =
    {program with functions= new_functions}

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (program : t)
        ~(globals : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(functions : Function.t id_assoc -> Function.t id_assoc M.t) :
        t M.t =
      Fields.fold ~init:(M.return program) ~globals:(F.proc_field globals)
        ~functions:(F.proc_field functions)
  end

  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.Named.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Initialiser.Named

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module L = Tx.List.On_monad (M)
      module F = Function.On_decls.On_monad (M)

      let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
        B.bmap program ~globals:(L.map_m ~f)
          ~functions:
            (L.map_m ~f:(fun (k, v) -> M.(F.map_m ~f v >>| Tuple2.create k)))
    end
  end)

  let cvars (prog : t) : Ac.C_id.Set.t =
    prog |> On_decls.to_list |> List.map ~f:fst |> Ac.C_id.Set.of_list
end
