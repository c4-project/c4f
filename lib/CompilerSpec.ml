open Core
open Sexplib

(** [CompilerSpec.t] describes how to invoke a compiler. *)
type t =
  { style : string
  ; emits : string
  ; cmd   : string
  ; argv  : string list
  } [@@deriving sexp]

type set = (string, t) List.Assoc.t [@@deriving sexp]

let pp (spec : t) (f : Format.formatter) : unit =
  Format.pp_open_vbox f 0;
  MyFormat.pp_kv "Style" (fun f -> Format.pp_print_string f spec.style) f;
  Format.pp_print_cut f ();
  MyFormat.pp_kv "Emits" (fun f -> Format.pp_print_string f spec.emits) f;
  Format.pp_print_cut f ();
  MyFormat.pp_kv "Command" (fun f -> Format.pp_print_string f spec.cmd) f;
  Format.pp_print_cut f ();
  MyFormat.pp_kv "Arguments"
                 (fun f -> Format.pp_print_list
                             ~pp_sep:(Format.pp_print_space)
                             (Format.pp_print_string)
                             f spec.argv) f;
  Format.pp_close_box f ()

let load_specs (specpath : string) : set =
  Sexp.load_sexp_conv_exn specpath set_of_sexp
