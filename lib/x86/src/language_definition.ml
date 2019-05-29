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

open Act_common
open Base
open Travesty_containers
include Language_definition_intf

module Common = struct
  let make_heap_loc l =
    Ast.(Location.Indirect (Indirect.make ~disp:(Disp.Symbolic l) ()))

  let indirect_abs_type (i : Indirect.t) =
    let open Act_abstract.Location in
    let open Indirect in
    match (seg i, disp i, base i, index i) with
    | None, disp, Some b, None ->
        let reg = Reg.abstract b in
        let offset =
          Option.value_map disp ~f:Disp.abstract
            ~default:(Act_abstract.Address.Int 0)
        in
        Act_abstract.Location.Register_indirect {reg; offset}
    (* This may be over-optimistic. *)
    | None, Some d, None, None ->
        Act_abstract.Location.Heap (Disp.abstract d)
    | _, _, _, _ ->
        Unknown

  module Abstract_location = Act_abstract.Abstractable.Make (struct
    type nonrec t = Ast.Location.t

    module Abs = Act_abstract.Location

    let abstract = function
      | Ast.Location.Reg reg ->
          Abs.Register_direct (Reg.abstract reg)
      | Indirect i ->
          indirect_abs_type i
      | Template_token _ ->
          Unknown
  end)

  module Symbol = struct
    include String

    let require_of_string = Or_error.return

    let abstract = Fn.id

    let abstract_demangle str =
      (* These are the types of manglings we've seen in practice: *)
      List.filter_opt
        [ Some str (* GNU/Linux ELF *)
        ; String.chop_prefix ~prefix:"_" str
          (* Darwin Mach-O *) ]

    module On_strings = Travesty.Traversable.Fix_elt (Singleton) (String)
  end

  module Statement = struct
    type t = Ast.Statement.t [@@deriving sexp, equal]

    let empty () = Ast.Statement.Nop

    let label s = Ast.Statement.Label s

    let instruction = Ast.Statement.instruction

    (* Statement abstraction is dialect-dependent, so we don't define it
       here. *)

    module On_symbols = Ast.Statement.On_symbols
    module On_instructions = Ast.Statement.On_instructions
  end

  module Location = struct
    type t = Ast.Location.t [@@deriving sexp, eq]

    let make_heap_loc = make_heap_loc

    module Sym = Symbol
    include Abstract_location
    module On_symbols = Ast.Location.On_symbols
  end

  module Program = struct
    type t = Ast.t [@@deriving sexp, equal]

    let name = Fn.const None

    module On_listings = Ast.On_listings

    let split prog ~f =
      let open Or_error.Let_syntax in
      let body = Ast.program prog in
      let%map bodies' = f body in
      List.map bodies' ~f:(fun body' ->
          On_listings.map prog ~f:(fun _ -> body') )
  end

  module Basic = struct
    let name = "X86"

    module Symbol = Symbol
  end
end

module Make_basic (T : Dialect_intf.S) (P : Pp_intf.S) = struct
  include Common.Basic

  let pp_comment = P.pp_comment

  module Location = struct
    include Common.Location

    let pp = P.pp_location
  end

  module Instruction = Language_instruction.Make (struct
    module Dialect = T
    module Pretty = P
    module Symbol = Symbol
    module Location = Location
  end)

  module Statement = struct
    include Common.Statement

    let pp = P.pp_statement

    include Act_abstract.Abstractable.Make (struct
      type nonrec t = Ast.Statement.t

      module Abs = Act_abstract.Statement
      open Abs

      let abstract = function
        | Ast.Statement.Instruction {opcode= Opcode.Directive s; _} ->
            Abs.Directive s
        | Instruction i ->
            Instruction (Instruction.abstract i)
        | Label l ->
            Label l
        | Nop ->
            Blank
    end)
  end

  module Program = struct
    include Common.Program

    let pp = P.pp
  end

  module Constant = struct
    (* TODO: this is too weak *)
    include Ast.Operand

    let pp = P.pp_operand

    let of_int (k : int) = Ast.Operand.Immediate (Disp.Numeric k)
  end
end

module Make (T : Dialect_intf.S) (P : Pp_intf.S) : S = struct
  module Dialect = T
  module Basic = Make_basic (T) (P)
  include Act_language.Definition.Make (Basic)
end

module Att = Make (Dialect.Att) (Pp.Att)
module Gcc = Make (Dialect.Gcc) (Pp.Gcc)
module Intel = Make (Dialect.Intel) (Pp.Intel)
module Herd7 = Make (Dialect.Herd7) (Pp.Herd7)

let dialect_table : (Id.t, (module S)) List.Assoc.t Lazy.t =
  lazy
    [ (Id.of_string "att", (module Att))
    ; (Id.of_string "gcc", (module Gcc))
    ; (Id.of_string "intel", (module Intel))
    ; (Id.of_string "herd7", (module Herd7)) ]

let of_dialect : Id.t -> (module S) Or_error.t =
  Staged.unstage
    (Dialect.find_by_id dialect_table
       ~context:"finding a language definition")
