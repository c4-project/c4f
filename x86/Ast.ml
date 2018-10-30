(*
This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor
   (parts (c) 2010-2018 Institut National de Recherche en Informatique et
	                en Automatique, Jade Alglave, and Luc Maranget)

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

This file derives from the Herd7 project
(https://github.com/herd/herdtools7); its original attribution and
copyright notice follow. *)

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Core
open Utils

(** [Fold_helpers] contains utility functions for building monadic
    [Fold_map]s. *)
module Fold_helpers (M : Monad.S) = struct
  (** [proc_variant0 f init] lifts a fold-map operation into
      a fold-map operation over a nullary variant constructor, using the
      Variantslib folder. *)
  let proc_variant0
    (f : 's -> unit -> ('s * unit) M.t)
    (init : 's)
    (v : 'a Base.Variant.t)
    : ('s * 'a) M.t =
    M.(f init () >>| (fun (s, ()) -> (s, v.Base.Variant.constructor)))
  ;;

  (** [proc_variant1 f init] lifts a fold-map operation into a
     fold-map operation over a unary variant constructor, using the
     Variantslib folder. *)
  let proc_variant1
      (f : 's -> 'a -> ('s * 'a) M.t)
      (init : 's)
      (v : ('a -> 'b) Base.Variant.t)
      (a : 'a)
    : ('s * 'b) M.t =
    M.(f init a >>| Tuple2.map_snd ~f:v.Base.Variant.constructor)
  ;;

  (** [proc_variant2 f g init] lifts a fold-map operation into a
     fold-map operation over a binary variant constructor, using the
     Variantslib folder. *)
  let proc_variant2
      (f : 's -> ('a * 'b) -> ('s * ('a * 'b)) M.t)
      (init : 's)
      (v : ('a -> 'b -> 'c) Base.Variant.t)
      (a : 'a)
      (b : 'b)
    : ('s * 'c) M.t =
    let open M.Let_syntax in
    let%map (init, (a', b')) = f init (a, b) in
    (init, v.Base.Variant.constructor a' b')
  ;;

  (** [proc_variant3 f g init] lifts a fold-map operation into a
     fold-map operation over a ternary variant constructor, using the
     Variantslib folder.  The functions get applied in order. *)
  let proc_variant3
      (f : 's -> ('a * 'b * 'c) -> ('s * ('a * 'b * 'c)) M.t)
      (init : 's)
      (v : ('a -> 'b -> 'c -> 'd) Base.Variant.t)
      (a : 'a)
      (b : 'b)
      (c : 'c)
    : ('s * 'd) M.t =
    let open M.Let_syntax in
    let%map (init, (a', b', c')) = f init (a, b, c) in
    (init, v.Base.Variant.constructor a' b' c')
  ;;

  let proc_field
      (f : 's -> 'a -> ('s * 'a) M.t)
      (acc : ('s * 'b) M.t)
      (field : ([> `Set_and_create], 'b, 'a) Field.t_with_perm)
      (_orig : 'b)
      (cval : 'a)
    : ('s * 'b) M.t =
    let open M.Let_syntax in
    let%bind (state,  container) = acc in
    let%map  (state', nval) = f state cval in
    (state', Field.fset field container nval)
  ;;

  let fold_nop
      (state : 'b)
      (item : 'a)
    : ('b * 'a) M.t =
    M.return (state, item)
  ;;

  module On_elt (Elt : Equal.S) = struct
    module F_opt    = Fold_map.Option (Elt)
    module F_opt_M  = F_opt.On_monad (M)
    module F_list   = Fold_map.List (Elt)
    module F_list_M = F_list.On_monad (M)

    let fold_opt = F_opt_M.fold_map
    let fold_list = F_list_M.fold_map
  end
end

module Reg = struct
  module M = struct
    (* Ordered as in Intel manual *)
    type t =
      | AH | AL | AX | EAX
      | BH | BL | BX | EBX
      | CH | CL | CX | ECX
      | DH | DL | DX | EDX
      | BP | EBP
      | SI | ESI
      | DI | EDI
      | SP | ESP
      | CS | DS | SS | ES | FS | GS
      | CF | PF | AF | ZF | SF | OF
      | EIP
    [@@deriving enum, sexp]
    ;;

    let table =
      [ AH , "AH"
      ; AL , "AL"
      ; AX , "AX"
      ; EAX, "EAX"
      ; BH , "BH"
      ; BL , "BL"
      ; BX , "BX"
      ; EBX, "EBX"
      ; CH , "CH"
      ; CL , "CL"
      ; CX , "CX"
      ; ECX, "ECX"
      ; DH , "DH"
      ; DL , "DL"
      ; DX , "DX"
      ; EDX, "EDX"
      ; BP , "BP"
      ; EBP, "EBP"
      ; SI , "SI"
      ; ESI, "ESI"
      ; DI , "DI"
      ; EDI, "EDI"
      ; SP , "SP"
      ; ESP, "ESP"
      ; CS , "CS"
      ; DS , "DS"
      ; SS , "SS"
      ; ES , "ES"
      ; FS , "FS"
      ; GS , "GS"
      ; CF , "CF"
      ; PF , "PF"
      ; AF , "AF"
      ; ZF , "ZF"
      ; SF , "SF"
      ; OF , "OF"
      ; EIP, "EIP"
      ]
    ;;
  end

  include M
  include Enum.ExtendTable (M)

  type kind =
    | Gen8 of [`Low | `High]
    | Gen16
    | Gen32
    | Segment
    | Flags
    | IP
  ;;

  let kind_of : t -> kind = function
    | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP -> Gen32
    | AX | BX | CX | DX | SI | DI | BP | SP -> Gen16
    | AH | BH | CH | DH -> Gen8 `High
    | AL | BL | CL | DL -> Gen8 `Low
    | CS | DS | SS | ES | FS | GS -> Segment
    | CF | PF | AF | ZF | SF | OF -> Flags
    | EIP -> IP
  ;;
end

module Disp = struct
  type t =
    | Symbolic of string
    | Numeric of int
  [@@deriving sexp, variants, eq]

  (** Base mapper for displacements *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let fold_map
        ~init
        ~symbolic ~numeric
        (x : t) : ('a * t) M.t =
      Variants.map x
        ~symbolic:(F.proc_variant1 symbolic init)
        ~numeric:(F.proc_variant1 numeric init)
    ;;
  end

  module On_symbols : Fold_map.S with type t := t and module Elt := String =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = String

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)
        module G = F.On_elt (Elt)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~symbolic:f
            (* Numeric displacements, of course, have no symbols *)
            ~numeric:F.fold_nop
        ;;
      end
    end)
end

module Index = struct
  type t =
    | Unscaled of Reg.t
    | Scaled of Reg.t * int
  [@@deriving sexp, variants, eq]

  (** Base mapper for indices *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let fold_map
        ~init
        ?(unscaled=F.fold_nop)
        ?(scaled=F.fold_nop)
        (x : t) : ('a * t) M.t =
      Variants.map x
        ~unscaled:(F.proc_variant1 unscaled init)
        ~scaled:(F.proc_variant2 scaled init)
    ;;
  end

  (** Recursive mapper for registers *)
  module On_registers : Fold_map.S with type t := t and module Elt := Reg =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = Reg

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)
        module G = F.On_elt (Elt)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~unscaled:f
            ~scaled:M.(fun s (r, k) -> f s r >>| (fun (s, r') -> (s, (r', k))))
        ;;
      end
    end)
end

(*
 * Memory addresses
 *)

module Indirect = struct
  type t =
    { seg    : Reg.t   option
    ; disp   : Disp.t  option
    ; base   : Reg.t   option
    ; index  : Index.t option
    }
  [@@deriving sexp, eq, fields, make]

  (** Base mapper for memory addresses *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let fold_map
        ~init
        ~seg ~disp ~base ~index
        indirect =
      Fields.Direct.fold
        indirect
        ~init:M.(return (init, indirect))
        ~seg:(F.proc_field seg)
        ~disp:(F.proc_field disp)
        ~base:(F.proc_field base)
        ~index:(F.proc_field index)
    ;;
  end

  (** Recursive mapper for symbols *)
  module On_symbols : Fold_map.S with type t := t and module Elt := String =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = String
      module Set = String.Set

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module D = Disp.On_symbols.On_monad (M)
        module G = F.On_elt (Disp)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~disp:(fun init ->
                G.fold_opt ~f:(fun init -> D.fold_map ~f ~init)
                  ~init)
            (* Segments, bases, and indices have no symbols. *)
            ~seg:F.fold_nop
            ~base:F.fold_nop
            ~index:F.fold_nop
        ;;
      end
    end)

  (** Recursive mapper for registers *)
  module On_registers : Fold_map.S with type t := t and module Elt := Reg =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = Reg

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module GR = F.On_elt (Reg)

        module I = Index.On_registers.On_monad (M)
        module GI = F.On_elt (Index)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~seg:(fun init -> GR.fold_opt ~f ~init)
            ~base:(fun init -> GR.fold_opt ~f ~init)
            ~index:(fun init -> GI.fold_opt ~f:(fun init -> I.fold_map ~f ~init) ~init)
            (* Displacements have no registers. *)
            ~disp:F.fold_nop
        ;;
      end
    end)
  ;;
end

(*
 * Locations
 *)


module Location = struct
  type t =
    | Indirect of Indirect.t
    | Reg of Reg.t
  [@@deriving sexp, variants, eq]

  (** Base mapper for locations *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let fold_map ~init ~indirect ~reg
        x =
      Variants.map
        x
        ~indirect:(F.proc_variant1 indirect init)
        ~reg:(F.proc_variant1 reg init)
    ;;
  end

  module On_registers : Fold_map.S with type t := t and module Elt := Reg =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = Reg

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module I  = Indirect.On_registers.On_monad (M)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~indirect:(fun init -> I.fold_map ~f ~init)
            ~reg:f
        ;;
      end
    end)
  ;;

  module On_symbols : Fold_map.S with type t := t and module Elt := String =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = String

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module I  = Indirect.On_symbols.On_monad (M)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~indirect:(fun init -> I.fold_map ~f ~init)
            (* Registers don't have any symbols *)
            ~reg:F.fold_nop
        ;;
      end
    end)
  ;;
end

module Operand = struct
  type bop =
    | BopPlus
    | BopMinus
  [@@deriving sexp, eq]

  type t =
    | Location of Location.t
    | Immediate of Disp.t
    | String of string
    | Typ of string
    | Bop of t * bop * t
  [@@deriving sexp, variants, eq]

  (** Base mapper for operands *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let rec fold_map
        ~init
        ~location
        ~immediate
        ~string
        ~typ
        ~bop
        (x : t) : ('a * t) M.t =
      Variants.map
        x
        ~location:(F.proc_variant1 location init)
        ~immediate:(F.proc_variant1 immediate init)
        ~string:(F.proc_variant1 string init)
        ~typ:(F.proc_variant1 typ init)
        ~bop:(F.proc_variant3
                (fun init (l, b, r) ->
                   let open M.Let_syntax in
                   let%bind (init, l') = fold_map ~init ~location ~immediate ~string ~typ ~bop l in
                   let%bind (init, b') = bop init b in
                   let%map  (init, r') = fold_map ~init ~location ~immediate ~string ~typ ~bop r in
                   (init, (l', b', r')))
                init)

    ;;
  end

  (** Recursive mapper for locations in operands *)
  module On_locations : Fold_map.S with type t := t and module Elt := Location =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = Location

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~location:f
            (* These don't contain locations: *)
            ~immediate:F.fold_nop
            ~string:F.fold_nop
            ~typ:F.fold_nop
            ~bop:F.fold_nop (* NB: this folds over the operator *)
        ;;
      end
    end)

  (** Recursive mapper for symbols in operands *)
  module On_symbols : Fold_map.S with type t := t and module Elt := String =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = String
      module Set = String.Set

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module L = Location.On_symbols.On_monad (M)
        module D = Disp.On_symbols.On_monad (M)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~location:(fun init -> L.fold_map ~f ~init)
            ~immediate:(fun init -> D.fold_map ~f ~init)
            (* These don't contain symbols: *)
            ~string:F.fold_nop
            ~typ:F.fold_nop
            ~bop:F.fold_nop (* NB: this folds over the operator *)
        ;;
      end
    end)
  ;;

  let%expect_test "symbol fold over bop" =
    let ast =
      bop
        (bop
           (immediate (Disp.Symbolic "a"))
           BopPlus
           (immediate (Disp.Symbolic "b")))
        BopMinus
        (location
           (Location.Indirect (Indirect.make ~disp:(Disp.Symbolic "c") ())))
    in
    let f count sym = (count + 1), String.capitalize sym in
    let (total, ast') = On_symbols.fold_map ~f ~init:0 ast in
    Format.printf "@[<v>@[<h>Total:@ %d@]@,%a@]@."
      total
      Sexp.pp_hum [%sexp (ast' : t)];
    [%expect {|
      Total: 3
      (Bop (Bop (Immediate (Symbolic A)) BopPlus (Immediate (Symbolic B))) BopMinus
       (Location (Indirect ((seg ()) (disp ((Symbolic C))) (base ()) (index ()))))) |}]
  ;;
end

(*
 * Prefixes
 *)

type prefix =
  | PreLock
[@@deriving sexp, eq]

(*
 * Sizes
 *)

type size =
  | SByte
  | SWord
  | SLong
[@@deriving sexp, eq]

(*
 * Conditions
 *)

type inv_condition =
  [ `Above
  | `AboveEqual
  | `Below
  | `BelowEqual
  | `Carry
  | `Equal
  | `Greater
  | `GreaterEqual
  | `Less
  | `LessEqual
  | `Overflow
  | `Parity
  | `Sign
  | `Zero
  ]
[@@deriving sexp, eq]

type condition =
  [ inv_condition
  | `Not of inv_condition
  | `CXZero
  | `ECXZero
  | `ParityEven
  | `ParityOdd
  ]
[@@deriving sexp, eq]


module InvConditionTable =
  StringTable.Make
    (struct
      type t = inv_condition
      let table =
        [ `Above       , "a"
        ; `AboveEqual  , "ae"
        ; `Below       , "b"
        ; `BelowEqual  , "be"
        ; `Carry       , "c"
        ; `Equal       , "e"
        ; `Greater     , "g"
        ; `GreaterEqual, "ge"
        ; `Less        , "l"
        ; `LessEqual   , "le"
        ; `Overflow    , "o"
        ; `Parity      , "p"
        ; `Sign        , "s"
        ; `Zero        , "z"
        ]
    end)

(** [build_inv_condition (ic, s) builds, for an invertible condition
   C, string table entries for C and NC. *)
let build_inv_condition (ic, s) =
  [ ((ic :> condition), s)
  ; (`Not ic, "n" ^ s)
  ]

module ConditionTable =
  StringTable.Make
    (struct
      type t = condition
      let table =
        List.bind ~f:build_inv_condition InvConditionTable.table
        @
        [ `CXZero    , "cxz"
        ; `ECXZero   , "ecxz"
        ; `ParityEven, "pe"
        ; `ParityOdd , "po"
        ]
    end)

(*
 * Opcodes
 *)

(* To add a new opcode:

   1.  Add an entry for it into either 'sizable_opcode' or
   'basic_opcode', in both here and the MLI file.  It goes into
   'sizable_opcode' if, in AT&T syntax, it can have a size prefix.

   2.  Add the string representation into the appropriate opcode
   table.

   3.  Add classifications for the instruction in x86.Language.

   4.  Add legs to the pattern-matches in x86.Sanitiser.
*)

type sizable_opcode =
  [ `Add
  | `Call
  | `Cmp
  | `Mov
  | `Pop
  | `Push
  | `Ret
  | `Sub
  | `Xchg
  | `Xor
  ]
[@@deriving sexp, eq]

module SizableOpcodeTable =
  StringTable.Make
    (struct
      type t = sizable_opcode
      let table =
        [ `Add,  "add"
        ; `Call, "call"
        ; `Cmp,  "cmp"
        ; `Mov,  "mov"
        ; `Pop,  "pop"
        ; `Push, "push"
        ; `Ret,  "ret"
        ; `Sub,  "sub"
        ; `Xchg, "xchg"
        ; `Xor,  "xor"
        ]
    end)

module ATTSizeTable =
  StringTable.Make
    (struct
      type t = size
      let table =
        [ SByte, "b"
        ; SWord, "w"
        ; SLong, "l"
        ]
    end)

module ATTSizedOpcodeTable =
  StringTable.Make
    (struct
      type t = (sizable_opcode * size)

      let table =
        List.map
          ~f:(fun ((op, ops), (sz, szs)) -> ((op, sz), ops^szs))
          (List.cartesian_product SizableOpcodeTable.table
                                  ATTSizeTable.table)
    end)

type basic_opcode =
  [ sizable_opcode
  | `Leave
  | `Mfence
  | `Nop
  ]
[@@deriving sexp, eq]

module BasicOpcodeTable =
  StringTable.Make
    (struct
      type t = basic_opcode
      let table =
        (SizableOpcodeTable.table
         :> (basic_opcode, string) List.Assoc.t)
        @
        [ `Leave,  "leave"
        ; `Mfence, "mfence"
        ; `Nop,    "nop"
        ]
    end)

type opcode =
  | OpBasic of basic_opcode
  | OpSized of sizable_opcode * size
  | OpJump of condition option
  | OpDirective of string
  | OpUnknown of string
[@@deriving sexp, eq]

module JumpTable =
  StringTable.Make
    (struct
      type t = condition option

      (* Jump instructions are always jC for some condition C, except
         jmp. *)
      let f (x, s) = (Some x, "j" ^ s)
      let table = (None, "jmp") :: List.map ~f ConditionTable.table
    end)

(*
 * Instructions
 *)

module Instruction = struct
  module T = struct
    type t =
      { prefix   : prefix option
      ; opcode   : opcode
      ; operands : Operand.t list
      }
    [@@deriving sexp, fields, eq, make]
    ;;
  end
  include T

  (** Base mapper for instructions *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let fold_map
        ~init
        ~prefix ~opcode ~operands
        ins =
      Fields.Direct.fold
        ins
        ~init:M.(return (init, ins))
        ~prefix:(F.proc_field prefix)
        ~opcode:(F.proc_field opcode)
        ~operands:(F.proc_field operands)
    ;;
  end

  (** Recursive mapper for symbols in instructions *)
  module On_symbols : Fold_map.S with type t := t and module Elt := String =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = String
      module Set = String.Set

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module O  = Operand.On_symbols.On_monad (M)
        module OG = F.On_elt (Operand)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~operands:(fun init -> OG.fold_list ~f:(fun init -> O.fold_map ~f ~init) ~init)
            (* Prefixes and opcodes don't contain symbols. *)
            ~prefix:F.fold_nop
            ~opcode:F.fold_nop
        ;;
      end
    end)
  ;;

  (** Recursive mapper for locations in instructions *)
  module On_locations : Fold_map.S with type t := t and module Elt := Location =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = Location

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module O  = Operand.On_locations.On_monad (M)
        module OG = F.On_elt (Operand)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~operands:(fun init -> OG.fold_list ~f:(fun init -> O.fold_map ~f ~init) ~init)
            (* Prefixes and opcodes don't contain locations. *)
            ~prefix:F.fold_nop
            ~opcode:F.fold_nop
        ;;
      end
    end)
  ;;
end

module Statement = struct
  type t =
    | Instruction of Instruction.t
    | Label of string
    | Nop
  [@@deriving sexp, eq, variants]

  (** Base mapper for statements *)
  module Base_map (M : Monad.S) = struct
    module F = Fold_helpers (M)

    let fold_map ~init ~instruction ~label ~nop x =
      Variants.map x
        ~instruction:(F.proc_variant1 instruction init)
        ~label:(F.proc_variant1 label init)
        ~nop:(F.proc_variant0 nop init)
    ;;
  end

  (** Recursive mapper for instructions in statements *)
  module On_instructions : Fold_map.S with type t := t and module Elt := Instruction =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = Instruction

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module I = Instruction.On_symbols.On_monad (M)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~instruction:f
            (* These don't contain instructions: *)
            ~label:F.fold_nop
            ~nop:F.fold_nop
        ;;
      end
    end)

  (** Recursive mapper for symbols in statements *)
  module On_symbols : Fold_map.S with type t := t and module Elt := String =
    Fold_map.Make (struct
      type nonrec t = t
      module Elt = String

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module F = Fold_helpers (M)

        module I = Instruction.On_symbols.On_monad (M)

        let fold_map ~f ~init t =
          B.fold_map t
            ~init
            ~instruction:(fun init -> I.fold_map ~f ~init)
            ~label:f
            (* These don't contain symbols: *)
            ~nop:F.fold_nop
        ;;
      end
    end)
end

(** [t] is the type of an X86 abstract syntax tree, containing the
    specific X86 syntax dialect and a list of statements. *)
type t =
  { syntax  : Dialect.t
  ; program : Statement.t list
  }
[@@deriving sexp, eq, fields]
