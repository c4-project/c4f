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
  val herd : t -> string option
  val machine : t -> Mach.t

  val create
    :  enabled : bool
    -> style   : string
    -> emits   : string list
    -> cmd     : string
    -> argv    : string list
    -> herd    : string option
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
      ; herd    : string sexp_option
      ; machine : Mach.t [@default Mach.default]
      } [@@deriving sexp, fields]

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
      MyFormat.pp_kv f "Machine" Mach.pp spec.machine;
      Format.pp_print_cut f ();
      MyFormat.pp_kv f "Herd" pp_herd_stanza spec.herd;
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
 * Cfg
 *)

module type CfgIntf = sig
  module C : CSpecIntf
  module M : MSpecIntf

  type t

  include Sexpable.S with type t := t

  val compilers : t -> C.Set.t
  val machines : t -> M.Set.t
end

module RawCfg = struct
  module CI : CfgIntf with module C = CfgCSpec and module M = MSpec = struct
    module C = CfgCSpec
    module M = MSpec

    type t =
      { compilers : C.Set.t
      ; machines  : M.Set.t
      }
    [@@deriving sexp, fields]
    ;;
  end
  include CI

  let load ~path =
    Or_error.(
      tag ~tag:"Couldn't parse compiler spec file."
        (try_with
           (fun () -> Sexp.load_sexp_conv_exn path [%of_sexp: CI.t]))
    )
  ;;
end

let part_chain_fst f g x =
  match f x with
  | `Fst y -> g y
  | `Snd y -> `Snd y
;;

module Cfg = struct
  module C = CSpec
  module M = MSpec

  type t =
    { compilers          : C.Set.t
    ; machines           : M.Set.t
    ; disabled_compilers : (Id.t, Error.t option) List.Assoc.t
    ; disabled_machines  : (Id.t, Error.t option) List.Assoc.t
    }
  [@@deriving sexp, fields]
  ;;

  (** [part_enabled x] is a partition_map function that
      sorts [x] into [`Fst] if they're enabled and [`Snd] if not. *)
  let part_enabled
      (type w) (module S : SpecIntf with type WithId.t = w)
      (x : S.WithId.t) =
    if (S.enabled (S.WithId.spec x))
    then `Fst x
    else `Snd (S.WithId.id x, None)
  ;;

  (** [part_hook hook x] is a partition_map function that
      runs [hook] on [x], and sorts the result into [`Fst] if it
      succeeded and [`Snd] if not. *)
  let part_hook
      (type w) (module S : SpecIntf with type WithId.t = w)
      (hook : S.WithId.t -> S.WithId.t option Or_error.t)
      (x : S.WithId.t) =
    match hook x with
    | Result.Ok (Some x') -> `Fst x'
    | Result.Ok None      -> `Snd (S.WithId.id x, None)
    | Result.Error err    -> `Snd (S.WithId.id x, Some err)
  ;;

  let machines_from_raw
      (hook : M.WithId.t -> M.WithId.t option Or_error.t)
      (ms : RawCfg.M.Set.t)
    : (M.Set.t * (Id.t, Error.t option) List.Assoc.t) Or_error.t =
    let open Or_error.Let_syntax in
    RawCfg.M.Set.(
      let enabled, disabled =
        partition_map
          ~f:(part_chain_fst
                (part_enabled (module M))
                (part_hook (module M) hook))
          ms
      in
      (** TODO(@MattWindsor91): test machines *)
      let%bind enabled' = M.Set.of_list enabled in
      return (enabled', disabled)
    )
  ;;

  let build_compiler
      (rawc : RawCfg.C.t)
      (mach : M.t)
    : C.t =
    RawCfg.C.(
      C.create
        ~enabled:(enabled rawc)
        ~style:(style rawc)
        ~emits:(emits rawc)
        ~cmd:(cmd rawc)
        ~argv:(argv rawc)
        ~herd:(herd rawc)
        ~machine:mach
    )
  ;;

  let find_machine enabled disabled mach =
    Or_error.(
      match M.Set.get enabled mach with
      | Ok m -> return (`Fst m)
      | _ ->
        match List.Assoc.find ~equal:Id.equal disabled mach with
        | Some e -> return (`Snd (mach, e))
        | None -> error "Machine doesn't exist" mach [%sexp_of:Id.t]
    )
  ;;

  let part_resolve enabled disabled c =
    let machid = RawCfg.C.machine (RawCfg.C.WithId.spec c) in
    match find_machine enabled disabled machid with
    | (* Machine enabled *)
      Result.Ok (`Fst mach) ->
      `Fst
        (C.WithId.create
           ~id:(RawCfg.C.WithId.id c)
           ~spec:(build_compiler (RawCfg.C.WithId.spec c) mach))
    | (* Machine disabled, possibly because of error *)
      Result.Ok (`Snd (_, err)) ->
      `Snd
        ( RawCfg.C.WithId.id c
        , Option.map
            ~f:(Error.tag ~tag:"Machine was disabled because:")
            err
        )
    | (* Error actually finding the machine *)
      Result.Error err ->
      `Snd
        ( RawCfg.C.WithId.id c
        , Some (Error.tag ~tag:"Error finding machine:" err)
        )
  ;;

  let compilers_from_raw
      (ms : M.Set.t)
      (ms_disabled : (Id.t, Error.t option) List.Assoc.t)
      (hook : C.WithId.t -> C.WithId.t option Or_error.t)
      (cs : RawCfg.C.Set.t)
    : (C.Set.t * (Id.t, Error.t option) List.Assoc.t) Or_error.t =
    let open Or_error.Let_syntax in
    RawCfg.C.Set.(
      let enabled, disabled =
        partition_map
          ~f:(
            part_chain_fst
              (* First, remove disabled compilers... *)
              (part_enabled (module CfgCSpec))
              (* ...then, resolve machine IDs and remove compilers with
                 no enabled machine... *)
              (part_chain_fst
                 (part_resolve ms ms_disabled)
                 (* ...then, run the given testing/filtering hook. *)
                 (part_hook (module C) hook))
          ) cs in
      (* TODO(@MattWindsor91): move compiler testing here. *)
      let%bind enabled' = C.Set.of_list enabled in
      return (enabled', disabled)
  )
  ;;

  let from_raw
      ?(chook=(Fn.compose Result.return Option.some))
      ?(mhook=(Fn.compose Result.return Option.some))
      (c : RawCfg.t)
    : t Or_error.t =
    let open Or_error.Let_syntax in
    let raw_ms = RawCfg.machines c in
    let%bind (machines, disabled_machines) = machines_from_raw mhook raw_ms in
    let raw_cs = RawCfg.compilers c in
    let%bind (compilers, disabled_compilers) =
      compilers_from_raw machines disabled_machines chook raw_cs in
    return
      { compilers
      ; machines
      ; disabled_compilers
      ; disabled_machines
      }
  ;;
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
