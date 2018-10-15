open Core
open Utils
open Utils.MyMonad

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

module Gcc (W : WithSpec) : Intf = struct
  let id = W.cid

  let compile ~infile ~outfile =
    let final_argv =
      [ "-S"       (* emit assembly *)
      ; "-fno-pic" (* don't emit position-independent code *)
      ]
      @ W.cspec.argv
      @ [ "-o"
        ; outfile
        ; infile
        ] in
    Run.run ~prog:W.cspec.cmd final_argv
  ;;

  let test () =
    Run.run ~prog:W.cspec.cmd ["--version"]
  ;;
end

let from_spec cid (cspec : CompilerSpec.t) =
  match cspec.style with
  | Gcc -> (module Gcc(
      struct
        let cid = cid
        let cspec = cspec
      end
      ) : Intf)
;;

let test_specs (specs : CompilerSpec.set) =
  let f (cid, cspec) =
    let module M = (val from_spec cid cspec) in
    Or_error.tag_arg
      (M.test ())
      "A compiler in your spec file didn't respond properly"
      cid
      [%sexp_of:CompilerSpec.Id.t]
  in
  MyOr_error.tapM
    ~f:(fun s -> Or_error.combine_errors_unit (List.map ~f s))
    specs

let load_and_test_specs ~path =
  let open Or_error.Let_syntax in
  CompilerSpec.load_specs ~path >>= test_specs
