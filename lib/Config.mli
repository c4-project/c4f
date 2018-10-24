(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

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
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Modules for dealing with act's top-level configuration *)

open Core

(** [Intf] is the baseline interface of modules over configuration. *)
module type Intf = sig
  module C : Compiler.CSpecIntf
  module M : Compiler.MSpecIntf

  type t [@@deriving sexp]

  (** [herd c] gets the Herd binary, if any, to use for configuration
      [c]. *)
  val herd : t -> string option;;

  (** [compilers c] gets the set of all active compilers in
      configuration [c]. *)
  val compilers : t -> C.Set.t;;

  (** [machines c] gets the set of all active machines in
      configuration [c]. *)
  val machines : t -> M.Set.t;;
end

(** [Raw] represents act configuration loaded directly from a spec
   file, without any compiler testing or expansion. *)
module Raw : sig
  include Intf with module C = Compiler.CfgCSpec
                and module M = Compiler.MSpec

  include Utils.Io.LoadableIntf with type t := t
end

  (** [M] represents fully processed act compiler configurations. *)
module M : sig
  include Intf with module C = Compiler.CSpec
                and module M = Compiler.MSpec

  (** ['t hook] is the type of testing hooks sent to [from_raw]. *)
  type 't hook = ('t -> 't option Or_error.t);;

  (** [herd_or_default c] behaves as [herd c], but substitutes a
      sensible default binary. *)
  val herd_or_default : t -> string;;

  (** [disabled_compilers c] reports all disabled compiler IDs in
      the given config, along with any reason why. *)
  val disabled_compilers : t -> (Compiler.Id.t * Error.t option) list

  (** [disabled_machines c] reports all disabled machines in
      the given config, along with any reason why. *)
  val disabled_machines : t -> (Compiler.Id.t * Error.t option) list

  (** [from_raw c ?chook ?mhook] takes a raw config [t] and processes it by:

      - applying the given testing hooks onto the compilers and machines, and
      disabling any that fail;
      - resolving machine references, and disabling any
      broken ones.

      Testing hooks are optional (and default to passing the compiler
      or machine through unaltered), and should return
      [Ok (Some x)] when the element is enabled and passing;
      [Ok None] when the element is disabled; and
      [Error e] when the element is enabled and failing. *)
  val from_raw
    :  ?chook:(Compiler.CSpec.WithId.t hook)
    -> ?mhook:(Compiler.MSpec.WithId.t hook)
    -> Raw.t
    -> t Or_error.t
  ;;
end
