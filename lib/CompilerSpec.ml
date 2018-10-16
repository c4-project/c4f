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
  } [@@deriving sexp, fields]

(*
 * t
 *)

module M = struct
  type t =
    { enabled : bool [@default true] [@sexp_drop_default]
    ; style : style
    ; emits : string sexp_list
    ; cmd   : string
    ; argv  : string sexp_list
    ; herd  : string sexp_option
    ; ssh   : ssh sexp_option
    } [@@deriving sexp, fields]

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
end
include M

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

module Set = struct
  type elt = M.t

  (* Wrapping this so that we can use [of_sexp] below. *)
  module SM = struct
    type t = (Id.t, M.t) List.Assoc.t [@@deriving sexp]
  end
  include SM

  let get specs cid =
    List.Assoc.find specs ~equal:(Id.equal) cid
    |> Result.of_option
      ~error:(Error.create "unknown compiler ID" cid [%sexp_of: Id.t])
  ;;

  let load ~path =
    Or_error.(
      tag ~tag:"Couldn't parse compiler spec file."
        (try_with
           (fun () -> Sexp.load_sexp_conv_exn path [%of_sexp: SM.t]))
    )
  ;;

  let filter ~f = List.filter ~f:(fun (_, e) -> f e);;

  let test
      ~(f : Id.t -> elt -> unit Or_error.t)
      (specs : t) : (t * Error.t list) =
    List.partition_map
      ~f:(fun (cid, cspec) ->
          match Result.error (f cid cspec) with
          | None -> `Fst (cid, cspec)
          | Some e -> `Snd e)
      (filter ~f:enabled specs)
  ;;

  let map ~f specs =
    List.map ~f:(Tuple2.uncurry f) (filter ~f:enabled specs)
  ;;

  let pp_spec_verbose f (c, s) =
    MyFormat.pp_kv f (Id.to_string c) M.pp s
  ;;

  let pp_spec_terse f (c, s) =
    Format.pp_open_hbox f ();
    let facts =
      List.concat
        [ [Id.to_string c]
        ; if s.enabled then [] else ["(DISABLED)"]
        ; if Option.is_none s.ssh then [] else ["(REMOTE)"]
        ]
    in
    Format.pp_print_list ~pp_sep:Format.pp_print_space String.pp f facts;
    Format.pp_close_box f ()
  ;;

  let pp_verbose
      (verbose : bool) (f : Format.formatter)
      (specs : t) : unit =
    Format.pp_open_vbox f 0;
    Format.pp_print_list
      ~pp_sep:Format.pp_print_cut
      (if verbose then pp_spec_verbose else pp_spec_terse)
      f
      specs;
    Format.pp_close_box f ();
    Format.pp_print_newline f ()
  ;;

  let pp = pp_verbose true;;
end

