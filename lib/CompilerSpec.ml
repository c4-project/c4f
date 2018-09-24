open Core
open Sexplib
open Utils

(*
 * style
 *)

type style =
  | Gcc
    [@@deriving show, sexp]

(*
 * arch
 *)

type arch =
  | X86
    [@@deriving show, sexp]

(*
 * t
 *)

type t =
  { style : style
  ; emits : arch
  ; cmd   : string
  ; argv  : string list
  } [@@deriving sexp]

type set = (string, t) List.Assoc.t [@@deriving sexp]

let pp f spec =
  Format.pp_open_vbox f 0;
  MyFormat.pp_kv f "Style" pp_style spec.style;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Emits" pp_arch spec.emits;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Command" Format.pp_print_string spec.cmd;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Arguments" (Format.pp_print_list
                                  ~pp_sep:(Format.pp_print_space)
                                  (Format.pp_print_string))
                             spec.argv;
  Format.pp_close_box f ()

let load_specs ~path =
  Sexp.load_sexp_conv_exn path set_of_sexp
