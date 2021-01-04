(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Helper functions and modules for enums *)

(** [Make_from_enumerate] makes an [S] from an [S_enumerate]. *)
module Make_from_enumerate (E : Enum_types.S_enumerate) :
  Enum_types.S with type t = E.t

(** [Extend] makes an enum extension. *)
module Extend (E : Enum_types.S_sexp) :
  Enum_types.Extension with type t := E.t

(** [Extend] makes an enum extension with table support. *)
module Extend_table (E : Enum_types.S_table) :
  Enum_types.Extension_table with type t := E.t
