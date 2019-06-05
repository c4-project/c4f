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

(** Argument specifications common to all act sub-commands. *)

open Core_kernel
open Act_common

include module type of Args_intf

(** {2 The standard arguments} *)

(** Module implementing a common way to retrieve and work with the act
    standard argument set. *)
module Standard : sig
  type t

  include S_standard with type t := t and type s := t
end

(** Variant of {{!Standard} Standard} including arguments for (optional)
    input and output files. This is useful for exposing a filter as an act
    command. *)
module Standard_with_files : S_standard_with_files with type s := Standard.t

(** Variant of {{!Standard_with_files} Standard_with_files} including the
    standard [act asm] arguments. *)
module Standard_asm : S_standard_asm with type s := Standard.t

(** {2 Miscellaneous argument helpers} *)

val flag_to_enum_choice :
  'a -> string -> doc:string -> 'a option Command.Param.t
(** [flag_to_enum_choice enum str ~doc] is a helper for implementing
    choose-one choices between multiple flags where each flag [str]
    corresponds to an enum variant [enum]. *)

val simulator :
  ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
(** [simulator ?name ?doc ()] produces a parameter, normally named
    [-simulator] but overridable by [name], that accepts a simulator ID. *)

val arch :
  ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
(** [arch ?name ?doc ()] produces a parameter, normally named [-arch] but
    overridable by [name], that accepts an architecture ID. *)

val file_type : Act_common.File_type.t Command.Param.t
(** [file_type] defines a parameter for specifying the file type of a single
    input file. *)

val c_globals : string list option Command.Param.t
(** [c_globals] defines a parameter for collecting a list of global C
    variables to track during sanitisation (and place in any generated
    locations stanzas). *)

val c_locals : string list option Command.Param.t
(** [c_locals] defines a parameter for collecting a list of local C
    variables to track during sanitisation. *)

val sanitiser_passes :
  Act_sanitiser.Pass_group.Selector.t Blang.t option Command.Param.t
(** [sanitiser_passes] defines a parameter for collecting a selector
    predicate for sanitiser passes. *)

val compiler_predicate :
  Act_compiler.Property.t Blang.t option Command.Param.t
(** [compiler_predicate] defines a parameter for collecting a filtering
    predicate for compilers. *)

val machine_predicate :
  Act_machine.Property.t Blang.t option Command.Param.t
(** [machine_predicate] defines a parameter for collecting a filtering
    predicate for machines. *)
