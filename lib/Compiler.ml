open Core
open Utils

module type Basic_spec = sig
  module Mach : Machine.Reference

  type t

  val enabled : t -> bool
  val style   : t -> string
  val emits   : t -> string list
  val cmd     : t -> string
  val argv    : t -> string list
  val herd    : t -> bool
  val machine : t -> Mach.t
end

module type Spec = sig
  include Basic_spec

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

  module With_id : sig
    include Spec.S_with_id with type elt = t
    include Basic_spec with type t := t and module Mach := Mach
  end

  include Spec.S with type t := t and module With_id := With_id
end

module Make_spec (R : Machine.Reference)
  : Spec with module Mach = R = struct
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
      My_format.pp_kv f "Style" String.pp spec.style;
      Format.pp_print_cut f ();
      My_format.pp_kv f "Emits"
        (Format.pp_print_list
           ~pp_sep:(Format.pp_print_space)
           String.pp)
        spec.emits;
      Format.pp_print_cut f ();
      My_format.pp_kv f "Command"
        (Format.pp_print_list ~pp_sep:(Format.pp_print_space) String.pp)
        (spec.cmd :: spec.argv);
      Format.pp_print_cut f ();
      My_format.pp_kv f "Machine" Mach.pp spec.machine;
      Format.pp_print_cut f ();
      My_format.pp_kv f "Herd"
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

(* This large amount of module juggling exists to let us be able to
   extend Spec.Make()'s With_id. *)
  module MS = Spec.Make (M)
  module Set = MS.Set
  include (M : Basic_spec with module Mach := Mach and type t := M.t)
  include (MS : Spec.Basic with type t := M.t)
  type t = M.t

  let create = M.Fields.create
  let pp_verbose = MS.pp_verbose

  module With_id = struct
    include MS.With_id

    let enabled w = M.enabled (spec w)
    let style   w = M.style   (spec w)
    let emits   w = M.emits   (spec w)
    let cmd     w = M.cmd     (spec w)
    let argv    w = M.argv    (spec w)
    let herd    w = M.herd    (spec w)
    let machine w = M.machine (spec w)
  end
end

module Cfg_spec : Spec with type Mach.t = Spec.Id.t =
  Make_spec (Machine.Id_as_reference)
;;

module Full_spec : Spec with type Mach.t = Machine.Spec.With_id.t =
  Make_spec (Machine.With_id_as_reference)
;;

module type With_spec = sig
  val cspec : Full_spec.With_id.t
end

module type Basic = sig
  val test_args : string list

  val compile_args
    :  args    : string list
    -> emits   : string list
    -> infile  : string
    -> outfile : string
    -> string list
end

module type Hooks = sig
  val pre : infile:string -> outfile:string -> (string * string) Or_error.t
  val post : infile:string -> outfile:string -> unit Or_error.t
end

(** [No_hooks] is a [Hooks] implementation that does nothing. *)
module No_hooks : Hooks = struct
  let pre ~infile ~outfile = Result.return (infile, outfile)
  let post ~infile ~outfile = ignore infile; ignore outfile; Result.ok_unit
end

(** [Scp_hooks] is a [Hooks] implementation that copies infile and outfile
    to and from a remote directory. *)
module Scp_hooks (C : sig val ssh: Machine.Ssh.t end) : Hooks = struct
  let remote_name_of file =
    Filename.concat (Machine.Ssh.copy_dir C.ssh) (Filename.basename file)
  ;;

  module Scp = Ssh.Scp (Machine.Ssh.To_config (C))

  let pre ~infile ~outfile =
    let open Or_error.Let_syntax in
    let%map () =
      Scp.send ~local:infile ~remote:(remote_name_of infile)
    in
    (remote_name_of infile, remote_name_of outfile)
  ;;

  let post ~infile ~outfile =
    ignore infile;
    Scp.receive ~remote:(remote_name_of outfile) ~local:outfile
  ;;
end

module type Basic_with_run_info = sig
  include Basic
  include With_spec
  module Runner : Run.Runner
  module Hooks : Hooks
end

module type S = sig
  val test : unit -> unit Or_error.t
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

module Make (B : Basic_with_run_info) : S = struct
  include B

  let cmd = Full_spec.cmd (Full_spec.With_id.spec B.cspec)

  let compile ~infile ~outfile =
    let open Or_error.Let_syntax in
    let%bind (infile', outfile') = B.Hooks.pre ~infile ~outfile in
    let s = Full_spec.With_id.spec B.cspec in
    let argv =
     B.compile_args
        ~args:(Full_spec.argv s)
        ~emits:(Full_spec.emits s) ~infile:infile' ~outfile:outfile'
    in
    let%bind () = B.Runner.run ~prog:cmd argv in
    (* NB: post intentionally gets sent the original filenames. *)
    B.Hooks.post ~infile ~outfile
  ;;

  let test () = B.Runner.run ~prog:cmd B.test_args
end

let runner_from_spec (cspec : Full_spec.t) =
  Machine.Spec.(
    runner (With_id.spec (Full_spec.machine cspec))
  )
;;

let hooks_from_spec (cspec : Full_spec.t) =
  match Machine.Spec.(via (With_id.spec (Full_spec.machine cspec))) with
  | Local -> (module No_hooks : Hooks)
  | Ssh s -> (module Scp_hooks (struct let ssh = s end) : Hooks)
;;

let from_spec f (cspec : Full_spec.With_id.t) =
  let open Or_error.Let_syntax in
  let%map s = f cspec in
  let h = hooks_from_spec (Full_spec.With_id.spec cspec) in
  let r = runner_from_spec (Full_spec.With_id.spec cspec) in
  (module
    (Make (struct
       let cspec = cspec
       include (val s : Basic)
       module Hooks = (val h : Hooks)
       module Runner = (val r : Run.Runner)
     end)) : S)
;;
