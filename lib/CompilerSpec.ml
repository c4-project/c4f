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
  ; emits : string sexp_list
  ; cmd   : string
  ; argv  : string sexp_list
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

let pp_ssh_stanza f =
  function
  | None -> String.pp f "local"
  | Some { host; user = Some u; copy_dir } ->
    Format.fprintf f "%s@@%s:%s" host u copy_dir
  | Some { host; user = None; copy_dir } ->
    Format.fprintf f "%s:%s" host copy_dir
;;

let pp_herd_stanza f =
  function
  | None -> String.pp f "no"
  | Some h -> Format.fprintf f "yes:@ %s" h
;;

let pp f spec =
  Format.pp_open_vbox f 0;
  if not spec.enabled then Format.fprintf f "-- DISABLED --@,";
  MyFormat.pp_kv f "Style" pp_style spec.style;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Emits"
    (Format.pp_print_list
       ~pp_sep:(Format.pp_print_space)
       String.pp)
    spec.emits;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Command"
    (Format.pp_print_list ~pp_sep:(Format.pp_print_space) String.pp)
    (spec.cmd :: spec.argv);
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Host" pp_ssh_stanza spec.ssh;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "Herd" pp_herd_stanza spec.herd;
  Format.pp_close_box f ()
;;

let load_specs ~path =
  Or_error.(
    tag ~tag:"Couldn't parse compiler spec file."
      (try_with
         (fun () -> Sexp.load_sexp_conv_exn path [%of_sexp: set]))
  )
