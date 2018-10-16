open Core
open Utils

(** [Intf] is the interface of compiler modules. *)
module type Intf = sig
  (** [id] gets the identifier for this compiler. *)
  val id : CompilerSpec.Id.t

  (** [test ()] tests that the compiler is working. *)
  val test : unit -> unit Or_error.t

  (** [compile ~infile ~outfile] runs the compiler on [infile],
      emitting assembly to [outfile] and returning any errors that arise. *)
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

module type WithSpec = sig
  val cid : CompilerSpec.Id.t
  val cspec : CompilerSpec.t
end

(** [Hooks] is the signature of mechanisms used to tell compilers to
   do something before/after a compilation.

    The main use of this module is to set up file transfers when doing
   remote compilation. *)
module type Hooks = sig
  (** [pre ~infile ~outfile] is run before compiling, and, if the
      hook is successful, returns the new infile and outfile to use
      for the compilation. *)
  val pre : infile:string -> outfile:string -> (string * string) Or_error.t

  (** [post ~infile ~outfile] is run after a successful compile.
      It receives the *original* input and output file names---not the
      ones returned by [pre]. *)
  val post : infile:string -> outfile:string -> unit Or_error.t
end

(** [NoHooks] is a [Hooks] implementation that does nothing. *)
module NoHooks : Hooks = struct
  let pre ~infile ~outfile = Result.return (infile, outfile)
  let post ~infile ~outfile = ignore infile; ignore outfile; Result.ok_unit
end

(** [ScpHooks] is a [Hooks] implementation that copies infile and outfile
    to and from a remote directory. *)
module ScpHooks (C : sig val ssh: CompilerSpec.ssh end) : Hooks = struct
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

module Gcc
    (S : WithSpec)
    (H : Hooks)
    (R : Run.Runner) : Intf = struct
  let id = S.cid

  let argv infile outfile =
    [ "-S"       (* emit assembly *)
    ; "-fno-pic" (* don't emit position-independent code *)
    ]
    @ S.cspec.argv
    @ [ "-o"; outfile; infile]
  ;;

  let compile ~infile ~outfile =
    let open Or_error.Let_syntax in
    let%bind (infile', outfile') = H.pre ~infile ~outfile in
    let%bind _ = R.run ~prog:S.cspec.cmd (argv infile' outfile') in
    H.post ~infile ~outfile
  ;;

  let test () =
    R.run ~prog:S.cspec.cmd ["--version"]
  ;;
end

let runner_from_spec (cspec : CompilerSpec.t) =
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

let hooks_from_spec (cspec : CompilerSpec.t) =
  match cspec.ssh with
  | None -> (module NoHooks : Hooks)
  | Some s -> (module ScpHooks (struct let ssh = s end) : Hooks)

let from_spec cid (cspec : CompilerSpec.t) =
  let module R = (val runner_from_spec cspec) in
  let module H = (val hooks_from_spec cspec) in
  match cspec.style with
  | Gcc -> (module Gcc(
      struct
        let cid = cid
        let cspec = cspec
      end
      )(H)(R) : Intf)
;;

let test_spec cid cspec =
  let module M = (val from_spec cid cspec) in
  Or_error.tag_arg
    (M.test ())
    "A compiler in your spec file didn't respond properly"
    cid
    [%sexp_of:CompilerSpec.Id.t]

let test_specs specs =
  CompilerSpec.Set.test ~f:test_spec specs
