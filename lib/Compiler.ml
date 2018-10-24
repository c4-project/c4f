open Core
open Utils

module Id = struct
  module T = struct
    (** [t] is the type of compiler IDs. *)
    type t = string list [@@deriving compare, hash, sexp, bin_io]

    let allowed_id_splits = [ '.' ; ' '; '/'; '\\']

    let of_string =
      String.split_on_chars ~on:allowed_id_splits

    let to_string =
      String.concat ~sep:"."

    let module_name = "act.Lib.Compiler.Id"
  end

  include T
  include Identifiable.Make (T)

  let to_string_list = Fn.id
end

module type SpecS = sig
  type t

  val enabled : t -> bool

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  val pp_summary : Format.formatter -> t -> unit
end

module type SpecIntf = sig
  include SpecS

  module WithId : sig
    type elt = t
    type t

    val create : id:Id.t -> spec:elt -> t;;
    val id : t -> Id.t;;
    val spec : t -> elt;;

    include Sexpable.S with type t := t
  end

  module Set : sig
    type elt = t
    type t

    include Pretty_printer.S with type t := t
    include Sexpable.S with type t := t

    val pp_verbose : bool -> Format.formatter -> t -> unit
    val get : t -> Id.t -> elt Or_error.t
    val of_list : WithId.t list -> t Or_error.t
    val partition_map
      :  t
      -> f : (WithId.t -> [`Fst of 'a | `Snd of 'b])
      -> ('a list * 'b list)
    ;;
    val map
      :  t
      -> f : (WithId.t -> 'a)
      -> 'a list
    ;;
  end

  val pp_verbose : bool -> Format.formatter -> t -> unit
end

(** [MakeSpec] makes a [SpecIntf] from a [SpecS]. *)
module MakeSpec (S : SpecS) : SpecIntf with type t = S.t = struct
  include S

  module WithId = struct
    type elt = S.t

    type t =
      { id   : Id.t
      ; spec : S.t
      }
    [@@deriving fields, sexp]
    ;;

    let create = Fields.create;;
  end

  let pp_verbose verbose = if verbose then pp else pp_summary;;

  module Set = struct
    type elt = S.t

    (* Wrapping this so that we can use [of_sexp] below. *)
    module SM = struct
      type t = (Id.t, S.t) List.Assoc.t [@@deriving sexp]
      let partition_map t ~f =
        List.partition_map t
          ~f:(fun (id, spec) -> f (WithId.create ~id ~spec))
      ;;

      let map t ~f =
        List.map t
          ~f:(fun (id, spec) -> f (WithId.create ~id ~spec))
      ;;
    end
    include SM

    let get specs cid =
      List.Assoc.find specs ~equal:(Id.equal) cid
      |> Result.of_option
        ~error:(Error.create "unknown compiler ID" cid [%sexp_of: Id.t])
    ;;

    let of_list xs =
      let open Or_error.Let_syntax in
      let%bind _ =
        xs
        |> List.find_all_dups ~compare:(MyFn.on WithId.id Id.compare)
        |> List.map
          ~f:(fun x -> Or_error.error "duplicate ID" (WithId.id x) [%sexp_of: Id.t])
        |> Or_error.combine_errors_unit
      in
      return (List.map ~f:(fun x -> (WithId.id x, WithId.spec x)) xs)
    ;;

    let pp_id_spec f ~pp id spec =
      MyFormat.pp_kv f
        (Id.to_string id)
        pp spec
    ;;

    let pp_verbose verbose f specs =
      Format.pp_open_vbox f 0;
      Format.pp_print_list
        ~pp_sep:Format.pp_print_cut
        (fun f ->
           Tuple2.uncurry
           (pp_id_spec ~pp:(pp_verbose verbose) f))
        f
        specs;
      Format.pp_close_box f ();
    ;;

    let pp = pp_verbose true;;
  end
end

module type MRefIntf = sig
  type t

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  val default : t;;
  val is_remote : t -> bool;;
end

module type SshIntf = sig
  type t


  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  val host : t -> string;;
  val user : t -> string option;;
  val copy_dir : t -> string;;
end

module Ssh : SshIntf = struct
  type t =
    { host     : string
    ; user     : string sexp_option
    ; copy_dir : string
    } [@@deriving sexp, fields]
  ;;

  let pp f { host; user; copy_dir } =
    match user with
    | Some u -> Format.fprintf f "%s@@%s:%s" host u copy_dir
    | None   -> Format.fprintf f "%s:%s" host copy_dir
  ;;
end

module type MSpecIntf = sig
  type t

  type via =
    | Local
    | Ssh of Ssh.t
  ;;
  val via : t -> via;;

  (** Machine specifications are machine references... *)
  include MRefIntf with type t := t
  (** ...and specifications. *)
  include SpecIntf with type t := t
end

module MSpec : MSpecIntf = struct
  type via =
    | Local
    | Ssh of Ssh.t
  [@@deriving sexp]
  ;;

  let pp_via f = function
    | Local -> String.pp f "local"
    | Ssh s -> Ssh.pp f s
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

    let pp_summary = pp;; (* for now *)

    let is_via_remote = function
      | Local -> false
      (* Technically, if we're SSHing to loopback, this isn't true,
         but I suspect it doesn't matter. *)
      | Ssh _ -> true
    ;;

    let is_remote {via; _} = is_via_remote via;;
  end

  include MakeSpec(M)
  let via = M.via;;
  let is_remote = M.is_remote;;
  let default = { M.enabled = true; via = Local };;
end

module type CSpecIntf = sig
  module Mach : MRefIntf

  type t

  val style : t -> string
  val emits : t -> string list
  val cmd : t -> string
  val argv : t -> string list
  val herd : t -> bool
  val machine : t -> Mach.t

  val create
    :  enabled : bool
    -> style   : string
    -> emits   : string list
    -> cmd     : string
    -> argv    : string list
    -> herd    : bool
    -> machine : Mach.t
    -> t
  ;;

  include SpecIntf with type t := t
end

module MakeCSpec (R : MRefIntf)
  : CSpecIntf with module Mach = R = struct
  module Mach = R

  module M = struct
    type t =
      { enabled : bool [@default true] [@sexp_drop_default]
      ; style   : string
      ; emits   : string sexp_list
      ; cmd     : string
      ; argv    : string sexp_list
      ; herd    : bool [@default true] [@sexp_drop_default]
      ; machine : Mach.t [@default Mach.default]
      } [@@deriving sexp, fields]

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
      MyFormat.pp_kv f "Machine" Mach.pp spec.machine;
      Format.pp_print_cut f ();
      MyFormat.pp_kv f "Herd"
        (fun f x -> String.pp f (if x then "yes" else "no")) spec.herd;
      Format.pp_close_box f ()
    ;;

    let pp_summary f spec =
    Format.pp_open_hbox f ();
    let facts =
      List.concat
        [ if enabled spec then [] else ["(DISABLED)"]
        ; if R.is_remote (machine spec) then [] else ["(REMOTE)"]
        ]
    in
    Format.pp_print_list ~pp_sep:Format.pp_print_space String.pp f facts;
    Format.pp_close_box f ()
    ;;
  end

  include MakeSpec(M)
  (* Re-export bits of M that don't clash with MakeSpec(M). *)
  let style = M.style
  let emits = M.emits
  let cmd = M.cmd
  let argv = M.argv
  let herd = M.herd
  let machine = M.machine
  let create = M.Fields.create
end

module CfgCSpec : CSpecIntf with type Mach.t = Id.t =
  MakeCSpec (struct
    include Id
    let default = Id.of_string "default";;
    let is_remote = Fn.const false;;
  end)
;;

module CSpec : CSpecIntf with type Mach.t = MSpec.t =
  MakeCSpec (MSpec)
;;

module type WithCSpec = sig
  val cspec : CSpec.WithId.t;;
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
module ScpHooks (C : sig val ssh: Ssh.t end) : Hooks = struct
  let scp_to src dst =
    (* Core_extended has scp, but it only goes in one direction. *)
    Or_error.(
    try_with
      (fun () ->
         Core_extended.Shell.scp ?user:(Ssh.user C.ssh) ~host:(Ssh.host C.ssh) src dst
      )
    )

  let scp_stanza file =
    let host = Ssh.host C.ssh in
    (* TODO(@MattWindsor91): escaping? *)
    match Ssh.user C.ssh with
    | None -> sprintf "%s:%s" host file
    | Some u -> sprintf "%s@%s:%s" u host file
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
    Filename.concat (Ssh.copy_dir C.ssh) (Filename.basename file)
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
  include WithCSpec

  val test : unit -> unit Or_error.t
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

module Make (P : WithCSpec) (C : S) (H : Hooks) (R : Run.Runner)
  : Intf = struct

  include P

  let compile ~infile ~outfile =
    let open Or_error.Let_syntax in
    let%bind (infile', outfile') = H.pre ~infile ~outfile in
    let s = CSpec.WithId.spec P.cspec in
    let argv =
      C.compile_args
        ~args:(CSpec.argv s) ~emits:(CSpec.emits s) ~infile:infile' ~outfile:outfile'
    in
    let%bind _ = R.run ~prog:(CSpec.cmd s) argv in
    (* NB: H.post intentionally gets sent the original filenames. *)
    H.post ~infile ~outfile
  ;;

  let test () = R.run ~prog:(CSpec.cmd (CSpec.WithId.spec P.cspec)) C.test_args;;
end

let runner_from_spec (cspec : CSpec.t) =
  match MSpec.via (CSpec.machine cspec) with
  | Local -> Run.(module Local : Runner)
  | Ssh sc ->
    let module C = struct
      let host = Ssh.host sc
      let user = Ssh.user sc
    end
    in
    Run.(module Ssh (C) : Runner)
;;

let hooks_from_spec (cspec : CSpec.t) =
  match MSpec.via (CSpec.machine cspec) with
  | Local -> (module NoHooks : Hooks)
  | Ssh s -> (module ScpHooks (struct let ssh = s end) : Hooks)
;;

let from_spec f (cspec : CSpec.WithId.t) =
  let open Or_error.Let_syntax in
  let%bind s = f cspec in
  let h = hooks_from_spec (CSpec.WithId.spec cspec) in
  let r = runner_from_spec (CSpec.WithId.spec cspec) in
  return
    (module
      (Make
         (struct let cspec = cspec end)
         (val s : S)
         (val h : Hooks)
         (val r : Run.Runner))
      : Intf)
