open Core
open Lang
open Sexplib
open Utils

(*
 * style
 *)

type style =
  | Gcc
    [@@deriving show, sexp]

(*
 * ssh
 *)

type ssh =
  { host     : string
  ; user     : string sexp_option
  ; copy_dir : string
  } [@@deriving sexp]

(*
 * t
 *)

type t =
  { enabled : bool [@default true] [@sexp_drop_default]
  ; style : style
  ; emits : Language.name
  ; cmd   : string
  ; argv  : string list
  ; herd  : string sexp_option
  ; ssh   : ssh sexp_option
  } [@@deriving sexp]

module Id = struct
  module T = struct
    (** [t] is the type of compiler IDs. *)
    type t = string list [@@deriving compare, hash, sexp]

    let allowed_id_splits = [ '.' ; ' '; '/'; '\\']

    let of_string =
      String.split_on_chars ~on:allowed_id_splits

    let to_string =
      String.concat ~sep:"."

    let module_name = "act.Lib.CompilerSpec"
  end

  include T
  include Identifiable.Make_plain (T)
end

type set = (Id.t, t) List.Assoc.t [@@deriving sexp]

let pp f spec =
  Format.pp_open_vbox f 0;
  MyFormat.pp_kv f "Style" pp_style spec.style;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Emits" Language.pp_name spec.emits;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Command" Format.pp_print_string spec.cmd;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Arguments" (Format.pp_print_list
                                  ~pp_sep:(Format.pp_print_space)
                                  (Format.pp_print_string))
                             spec.argv;
  Format.pp_close_box f ()

let load_specs ~path =
  Or_error.(
    tag ~tag:"Couldn't parse compiler spec file."
      (try_with
         (fun () -> Sexp.load_sexp_conv_exn path [%of_sexp: set]))
  )
