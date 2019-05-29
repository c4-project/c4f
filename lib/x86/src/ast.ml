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
open Base_quickcheck
open Act_common
module Tx = Travesty_base_exts
module Au = Act_utils
open Travesty

module Location = struct
  type t =
    | Indirect of Indirect.t
    | Reg of Reg.t
    | Template_token of string
  [@@deriving sexp, variants, compare, equal, quickcheck]

  (** Base mapper for locations *)
  module Base_map (M : Monad.S) = struct
    module F = Traversable.Helpers (M)

    let map_m (x : t) ~(indirect : Indirect.t -> Indirect.t M.t)
        ~(reg : Reg.t -> Reg.t M.t) ~(template_token : string -> string M.t)
        : t M.t =
      Variants.map x
        ~indirect:(F.proc_variant1 indirect)
        ~reg:(F.proc_variant1 reg)
        ~template_token:(F.proc_variant1 template_token)
  end

  module On_registers :
    Traversable.S0 with type t = t and type Elt.t = Reg.t =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Reg

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)
      module I = Indirect.On_registers.On_monad (M)

      let map_m t ~f =
        B.map_m t ~indirect:(I.map_m ~f) ~reg:f ~template_token:M.return
    end
  end)

  module On_symbols :
    Traversable.S0 with type t = t and type Elt.t = string =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = String

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)
      module I = Indirect.On_symbols.On_monad (M)

      let map_m t ~f =
        B.map_m t ~indirect:(I.map_m ~f)
          ~reg:M.return (* Registers don't have any symbols *)
          ~template_token:M.return

      (* Template tokens don't have any (traversable!) symbols *)
    end
  end)
end

module Bop = struct
  module M = struct
    type t = Plus | Minus [@@deriving enum]

    let table = [(Plus, "+"); (Minus, "-")]
  end

  include M
  include Au.Enum.Extend_table (M)
end

module Operand = struct
  type t =
    | Location of Location.t
    | Immediate of Disp.t
    | String of string
    | Typ of string
    | Bop of t * Bop.t * t
  [@@deriving sexp, variants, compare, equal]

  (** Base mapper for operands *)
  module Base_map (M : Monad.S) = struct
    module F = Traversable.Helpers (M)

    let rec map_m (x : t) ~location ~immediate ~string ~typ ~bop : t M.t =
      Variants.map x
        ~location:(F.proc_variant1 location)
        ~immediate:(F.proc_variant1 immediate)
        ~string:(F.proc_variant1 string) ~typ:(F.proc_variant1 typ)
        ~bop:
          (F.proc_variant3 (fun (l, b, r) ->
               let open M.Let_syntax in
               let%bind l' =
                 map_m ~location ~immediate ~string ~typ ~bop l
               in
               let%bind b' = bop b in
               let%map r' =
                 map_m ~location ~immediate ~string ~typ ~bop r
               in
               (l', b', r') ))
  end

  (** Recursive mapper for locations in operands *)
  module On_locations :
    Traversable.S0 with type t = t and type Elt.t = Location.t =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Location

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)

      let map_m t ~f =
        B.map_m t ~location:f (* These don't contain locations: *)
          ~immediate:M.return ~string:M.return ~typ:M.return ~bop:M.return

      (* NB: this folds over the operator *)
    end
  end)

  (** Recursive mapper for symbols in operands *)
  module On_symbols :
    Traversable.S0 with type t = t and type Elt.t = string =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = String

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module L = Location.On_symbols.On_monad (M)
      module D = Disp.On_symbols.On_monad (M)

      let map_m t ~f =
        B.map_m t ~location:(L.map_m ~f)
          ~immediate:(D.map_m ~f) (* These don't contain symbols: *)
          ~string:M.return ~typ:M.return ~bop:M.return

      (* NB: this folds over the operator *)
    end
  end)

  module Q : Au.My_quickcheck.S_with_sexp with type t := t = struct
    let sexp_of_t = sexp_of_t

    module G = Base_quickcheck.Generator
    module O = Base_quickcheck.Observer
    module S = Base_quickcheck.Shrinker

    let anonymise = function
      | Location loc ->
          `A loc
      | Immediate disp ->
          `B disp
      | String str ->
          `C str
      | Typ typ ->
          `D typ
      | Bop (l, b, r) ->
          `E (l, b, r)

    let deanonymise = function
      | `A loc ->
          Location loc
      | `B disp ->
          Immediate disp
      | `C str ->
          String str
      | `D typ ->
          Typ typ
      | `E (l, b, r) ->
          Bop (l, b, r)

    let quickcheck_generator : t G.t =
      G.recursive_union
        [ G.map ~f:deanonymise
            [%quickcheck.generator:
              [`A of Location.t | `B of Disp.t | `C of string | `D of string]]
        ]
        ~f:(fun mu ->
          [ G.map
              ~f:(fun (l, b, r) -> Bop (l, b, r))
              [%quickcheck.generator: [%custom mu] * Bop.t * [%custom mu]]
          ] )

    let quickcheck_observer : t O.t =
      O.fixed_point (fun mu ->
          O.unmap ~f:anonymise
            [%quickcheck.observer:
              [ `A of Location.t
              | `B of Disp.t
              | `C of string
              | `D of string
              | `E of [%custom mu] * Bop.t * [%custom mu] ]] )

    let quickcheck_shrinker : t S.t =
      S.fixed_point (fun mu ->
          S.map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker:
              [ `A of Location.t
              | `B of Disp.t
              | `C of string
              | `D of string
              | `E of [%custom mu] * Bop.t * [%custom mu] ]] )
  end

  include Q

  let symbolic (body : string) : t = Immediate (Disp.symbolic body)

  let%expect_test "symbol fold over bop" =
    let ast =
      bop
        (bop (symbolic "a") Bop.Plus (symbolic "b"))
        Bop.Minus
        (location
           (Location.Indirect (Indirect.make ~disp:(Disp.Symbolic "c") ())))
    in
    let f count sym = (count + 1, String.capitalize sym) in
    let total, ast' = On_symbols.fold_map ~f ~init:0 ast in
    Fmt.pr "@[<v>@[<h>Total:@ %d@]@,%a@]@." total Sexp.pp_hum
      [%sexp (ast' : t)] ;
    [%expect
      {|
      Total: 3
      (Bop (Bop (Immediate (Symbolic A)) + (Immediate (Symbolic B))) -
       (Location (Indirect ((seg ()) (disp ((Symbolic C))) (base ()) (index ()))))) |}]
end

type prefix = PreLock [@@deriving sexp, eq]

module Instruction = struct
  module T = struct
    type t =
      {prefix: prefix option; opcode: Opcode.t; operands: Operand.t list}
    [@@deriving sexp, fields, eq, make]
  end

  include T

  (** Base mapper for instructions *)
  module Base_map (M : Monad.S) = struct
    module F = Traversable.Helpers (M)

    let map_m ins ~prefix ~opcode ~operands =
      Fields.fold ~init:(M.return ins) ~prefix:(F.proc_field prefix)
        ~opcode:(F.proc_field opcode) ~operands:(F.proc_field operands)
  end

  (** Recursive mapper for symbols in instructions *)
  module On_symbols :
    Traversable.S0 with type t = t and type Elt.t = string =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = String
    module Set = Set.M (String)

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)
      module OS = Operand.On_symbols.On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m t ~f =
        B.map_m t
          ~operands:
            (L.map_m ~f:(OS.map_m ~f))
            (* Prefixes and opcodes don't contain symbols. *)
          ~prefix:M.return ~opcode:M.return
    end
  end)

  (** Recursive mapper for locations in instructions *)
  module On_locations :
    Traversable.S0 with type t = t and type Elt.t = Location.t =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Location

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)
      module OL = Operand.On_locations.On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m t ~f =
        B.map_m t
          ~operands:
            (L.map_m ~f:(OL.map_m ~f))
            (* Prefixes and opcodes don't contain locations. *)
          ~prefix:M.return ~opcode:M.return
    end
  end)

  let op_on_label (opcode : Opcode.t) (label : string) : t =
    make ~opcode ~operands:[Operand.symbolic label] ()

  let jmp_label : string -> t = op_on_label Opcode.jmp

  let call_label : string -> t = op_on_label Opcode.call
end

module Statement = struct
  type t = Instruction of Instruction.t | Label of string | Nop
  [@@deriving sexp, eq, variants]

  (** Base mapper for statements *)
  module Base_map (M : Monad.S) = struct
    module F = Traversable.Helpers (M)

    let map_m x ~instruction ~label ~nop =
      Variants.map x
        ~instruction:(F.proc_variant1 instruction)
        ~label:(F.proc_variant1 label) ~nop:(F.proc_variant0 nop)
  end

  (** Recursive mapper for instructions in statements *)
  module On_instructions :
    Traversable.S0 with type t = t and type Elt.t = Instruction.t =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Instruction

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)
      module I = Instruction.On_symbols.On_monad (M)

      let map_m t ~f =
        B.map_m t ~instruction:f (* These don't contain instructions: *)
          ~label:M.return ~nop:M.return
    end
  end)

  (** Recursive mapper for symbols in statements *)
  module On_symbols :
    Traversable.S0 with type t = t and type Elt.t = string =
  Traversable.Make0 (struct
    type nonrec t = t

    module Elt = String

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module F = Traversable.Helpers (M)
      module I = Instruction.On_symbols.On_monad (M)

      let map_m t ~f =
        B.map_m t ~instruction:(I.map_m ~f)
          ~label:f (* These don't contain symbols: *) ~nop:M.return
    end
  end)
end

(* The ordering here is important to make sure [make] puts the optional
   'program' first. *)
type t = {program: Statement.t list; dialect: Id.t}
[@@deriving sexp, equal, fields, make]

(** Base mapper for ASTs *)
module Base_map (M : Monad.S) = struct
  module F = Traversable.Helpers (M)

  let map_m x ~dialect ~program =
    Fields.fold ~init:(M.return x) ~dialect:(F.proc_field dialect)
      ~program:(F.proc_field program)
end

module On_listings :
  Traversable.S0 with type t = t and type Elt.t = Statement.t list =
Traversable.Make0 (struct
  type nonrec t = t

  module Elt = struct
    type t = Statement.t list [@@deriving eq]
  end

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)

    let map_m t ~f = B.map_m t ~program:f ~dialect:M.return
  end
end)

module On_statements :
  Traversable.S0 with type t = t and type Elt.t = Statement.t =
  Traversable.Chain0
    (On_listings)
    (Traversable.Fix_elt (Tx.List) (Statement))
