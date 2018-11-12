(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core
open Utils

module type Reference = sig
  type t [@@deriving sexp]
  include Pretty_printer.S with type t := t
  val default : t
  val is_remote : t -> bool
end

module Ssh = struct
  type t =
    { host     : string
    ; user     : string sexp_option
    ; copy_dir : string
    } [@@deriving sexp, fields]
  ;;

  (* The use of 'sexp_option' above makes deriving this impossible. *)
  let create ~host ?user ~copy_dir = Fields.create ~host ~user ~copy_dir

  let pp f { host; user; copy_dir } =
    match user with
    | Some u -> Format.fprintf f "%s@@%s:%s" host u copy_dir
    | None   -> Format.fprintf f "%s:%s" host copy_dir
  ;;

  module To_config (C : sig val ssh : t end) : Ssh.S = struct
    let host = host C.ssh
    let user = user C.ssh
  end
end

module Spec = struct
  type via =
    | Local
    | Ssh of Ssh.t
  [@@deriving sexp]
  ;;

  let pp_via f = function
    | Local -> String.pp f "local"
    | Ssh s -> Ssh.pp f s
  ;;

  let runner_from_via = function
    | Local -> (module Run.Local : Run.Runner)
    | Ssh c -> (
        module Utils.Ssh.Runner (Ssh.To_config (struct let ssh = c end)) : Run.Runner
      )
  ;;

  module M = struct
    type t =
      { enabled : bool [@default true] [@sexp_drop_default]
      ; via     : via
      }
    [@@deriving sexp, fields]
    ;;

    let pp f {via; enabled} =
      Format.pp_open_hbox f ();
      pp_via f via;
      if not enabled then begin
        Format.pp_print_space f ();
        String.pp f "(DISABLED)";
      end;
      Format.pp_close_box f ()
    ;;

    let pp_summary = pp (* for now *)

    let is_via_remote = function
      | Local -> false
      (* Technically, if we're SSHing to loopback, this isn't true,
         but I suspect it doesn't matter. *)
      | Ssh _ -> true
    ;;

    let is_remote {via; _} = is_via_remote via
    let runner {via; _} = runner_from_via via
  end

  include Spec.Make(M)
  let via = M.via
  let runner = M.runner
  let is_remote = M.is_remote
  let default = { M.enabled = true; via = Local }
end

(** [Id_as_reference] is an extension onto [Id] that
    lets such items be machine references. *)
module Id_as_reference : sig
  include (module type of Id)
  include Reference with type t := t
end = struct
  include Id
  let default = of_string "default"
  let is_remote = Fn.const false
end

(** [With_id_as_reference] is an extension onto [Spec.With_id] that
    lets such items be machine references. *)
module With_id_as_reference : sig
  type elt = Spec.t
  type t = Spec.With_id.t
  include (module type of Spec.With_id with type elt := elt and type t := t)
  include Reference with type t := t
end = struct
  include Spec.With_id
  let is_remote t = Spec.is_remote (spec t)
  let default =
    create ~id:Id_as_reference.default ~spec:Spec.default
  let pp f t =
    Format.fprintf f "@[%a@ (@,%a@,)@]"
      Id.pp   (id   t)
      Spec.pp (spec t)
  ;;
end
