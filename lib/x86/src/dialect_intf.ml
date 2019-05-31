(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module signatures for dialects.

    These signatures cover the parts of the x86 syntax that are different
    between dialects, but don't manifest in the way the dialect is
    pretty-printed. *)

(** [Has_dialect] is a signature for modules that report a specific dialect. *)
module type Has_dialect = sig
  val dialect_id : Act_common.Id.t
  (** [dialect] is the identifier of this module's associated x86 dialect. *)
end

(** [Basic] is the basic interface of modules containing x86 dialect
    information. *)
module type Basic = sig
  include Has_dialect

  val readme : unit -> string
  (** [readme ()] gets a free-form, human-readable, description of this
      dialect. *)

  val operand_order : Act_common.Src_dst.order
  (** [operand_order] describes the order in which this dialect lays out
      source/dest instruction operands. *)

  val is_asm_template : bool
  (** [is_asm_template] describes whether this dialect supports template
      interpolations in its AST. *)

  val has_size_suffix : bool
  (** [has_size_suffix] gets whether this dialect uses AT&T-style size
      suffixes. *)

  val symbolic_jump_type : [`Indirect | `Immediate]
  (** [symbolic_jump_type] gets the type of syntax this dialect _appears_ to
      use for symbolic jumps.

      In all x86 dialects, a jump to a label is `jCC LABEL`, where `CC` is
      `mp` or some condition. Because of the way we parse x86, the label
      resolves to different abstract syntax depending on the dialect.

      In AT&T, symbolic jumps look like indirect displacements; in Intel and
      Herd7, they look like immediate values. *)
end

(** [S] is the type of fully-instantiated dialect modules. *)
module type S = sig
  include Basic

  include Act_common.Src_dst.S

  val make_jump_operand : string -> Ast.Operand.t
  (** [make_jump_operand sym] makes the appropriate form of a symbolic jump
      operand for this dialect. *)

  val call_to_symbol : string -> Ast.Instruction.t
  (** [call_to_symbol sym] generates the dialect-appropriate symbolic
      procedure call instruction towards [sym]. *)

  val jmp_to_symbol : string -> Ast.Instruction.t
  (** [jmp_to_symbol sym] generates the dialect-appropriate symbolic jump
      instruction towards [sym]. *)
end
