open Core
open Sexplib
open Utils

module Id = struct
  module T = struct
    (** [t] is the type of compiler IDs. *)
    type t = string list [@@deriving compare, hash, sexp]

    let allowed_id_splits = [ '.' ; ' '; '/'; '\\']

    let of_string =
      String.split_on_chars ~on:allowed_id_splits

    let to_string =
      String.concat ~sep:"."

    let module_name = "act.Lib.Compiler.Id"
  end

  include T
  include Identifiable.Make_plain (T)

  let to_string_list = Fn.id
end

module Spec = struct
  type ssh =
    { host     : string
    ; user     : string sexp_option
    ; copy_dir : string
    } [@@deriving sexp, fields]

  type t =
    { enabled : bool [@default true] [@sexp_drop_default]
    ; style : string
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
    MyFormat.pp_kv f "Style" String.pp spec.style;
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

  type with_id =
    { cid   : Id.t
    ; cspec : t
    }
end

module type WithSpec = sig
  val cspec : Spec.with_id
end

module Set = struct
  (* Wrapping this so that we can use [of_sexp] below. *)
  module SM = struct
    type t = (Id.t, Spec.t) List.Assoc.t [@@deriving sexp]
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
      ~(f : Spec.with_id -> unit Or_error.t)
      (specs : t) : (t * Error.t list) =
    List.partition_map
      ~f:(fun (cid, cspec) ->
          match Result.error (f {cid; cspec}) with
          | None -> `Fst (cid, cspec)
          | Some e -> `Snd e)
      (filter ~f:Spec.enabled specs)
  ;;

  let map
      ~(f : Spec.with_id -> 'a) specs =
    List.map ~f:(fun (cid, cspec) -> f {cid; cspec})
      (filter ~f:Spec.enabled specs)
  ;;

  let pp_spec_verbose f (c, s) =
    MyFormat.pp_kv f (Id.to_string c) Spec.pp s
  ;;

  let pp_spec_terse f (c, s) =
    Format.pp_open_hbox f ();
    let facts =
      List.concat
        [ [Id.to_string c]
        ; if Spec.enabled s then [] else ["(DISABLED)"]
        ; if Option.is_none (Spec.ssh s) then [] else ["(REMOTE)"]
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

(*
 * S
 *)

module type S = sig
  val test_args : string list

  val compile_args
    :  args    : string list
    -> emits   : string list
    -> infile  : string
    -> outfile : string
    -> string list
end

(*
 * Hooks
 *)

module type Hooks = sig
  val pre : infile:string -> outfile:string -> (string * string) Or_error.t
  val post : infile:string -> outfile:string -> unit Or_error.t
end

(** [NoHooks] is a [Hooks] implementation that does nothing. *)
module NoHooks : Hooks = struct
  let pre ~infile ~outfile = Result.return (infile, outfile)
  let post ~infile ~outfile = ignore infile; ignore outfile; Result.ok_unit
end

(** [ScpHooks] is a [Hooks] implementation that copies infile and outfile
    to and from a remote directory. *)
module ScpHooks (C : sig val ssh: Spec.ssh end) : Hooks = struct
  let scp_to src dst =
    (* Core_extended has scp, but it only goes in one direction. *)
    Or_error.(
    try_with
      (fun () ->
         Core_extended.Shell.scp ?user:C.ssh.user ~host:C.ssh.host src dst
      )
    )

  let scp_stanza file =
    (* TODO(@MattWindsor91): escaping? *)
    match C.ssh.user with
    | None -> sprintf "%s:%s" C.ssh.host file
    | Some u -> sprintf "%s@%s:%s" u C.ssh.host file
  ;;

  let scp_from src dst : unit Or_error.t =
    (* We need to make our own implementation of backwards scp,
       which is flaky. *)
    Run.Local.run
      ~prog:"scp"
      [ "-q" (* quiet mode *)
      ; "-B" (* batch mode *)
      ; scp_stanza src
      ; dst
      ]
  ;;

  let remote_name_of file =
    Filename.concat C.ssh.copy_dir (Filename.basename file)
  ;;

  let pre ~infile ~outfile =
    let open Or_error.Let_syntax in
    let%bind _ = scp_to infile (remote_name_of infile) in
    return (remote_name_of infile, remote_name_of outfile)
  ;;

  let post ~infile ~outfile =
    ignore infile;
    scp_from (remote_name_of outfile) outfile
  ;;
end

(*
 * Intf and making Intfs
 *)

module type Intf = sig
  include WithSpec

  val test : unit -> unit Or_error.t
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

module Make (P : WithSpec) (C : S) (H : Hooks) (R : Run.Runner)
  : Intf = struct

  include P

  let compile ~infile ~outfile =
    let open Or_error.Let_syntax in
    let%bind (infile', outfile') = H.pre ~infile ~outfile in
    let s = P.cspec.cspec in
    let argv =
      C.compile_args
        ~args:s.argv ~emits:s.emits ~infile:infile' ~outfile:outfile'
    in
    let%bind _ = R.run ~prog:s.cmd argv in
    (* NB: H.post intentionally gets sent the original filenames. *)
    H.post ~infile ~outfile
  ;;

  let test () = R.run ~prog:P.cspec.cspec.cmd C.test_args;;
end

let runner_from_spec (cspec : Spec.t) =
  Run.(
    match cspec.ssh with
    | None -> (module Local : Runner)
    | Some sc ->
      let module C = struct
        let host = sc.host
        let user = sc.user
      end
      in
      (module Ssh (C) : Runner)
  )
;;

let hooks_from_spec (cspec : Spec.t) =
  match cspec.ssh with
  | None -> (module NoHooks : Hooks)
  | Some s -> (module ScpHooks (struct let ssh = s end) : Hooks)

let from_spec f (cspec : Spec.with_id) =
  let open Or_error.Let_syntax in
  let%bind s = f cspec in
  let h = hooks_from_spec (cspec.cspec) in
  let r = runner_from_spec (cspec.cspec) in
  return
    (module
      (Make
         (struct let cspec = cspec end)
         (val s : S)
         (val h : Hooks)
         (val r : Run.Runner))
      : Intf)
