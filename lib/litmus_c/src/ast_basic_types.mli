(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Parts of this file ultimately derive from the Herdtools7 C AST, which has
   the following attribution:

   the diy toolsuite

   Jade Alglave, University College London, UK.

   Luc Maranget, INRIA Paris-Rocquencourt, France.

   Copyright 2010-present Institut National de Recherche en Informatique et
   en Automatique and the authors. All rights reserved.

   This software is governed by the CeCILL-B license under French law and by
   the rules of distribution of free software. You can use, and/ or
   redistribute the software under the terms of the CeCILL-B license as
   circulated by CEA, CNRS and INRIA at the following URL
   "http://www.cecill.info". We also give a copy in LICENSE.txt. *)

open Base

(** {2 General node signatures} *)

(** Signature of a basic AST node. *)
module type Ast_node = sig
  type t [@@deriving sexp, equal, compare]

  include Pretty_printer.S with type t := t
end

(** Signature of an AST node that holds exactly one identifier. *)
module type Ast_node_with_identifier = sig
  include Ast_node

  val identifier : t -> Act_common.C_id.t
end
