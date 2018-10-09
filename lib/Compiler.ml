open Core
open Utils

type compilation =
  { cc_id    : string
  ; cc_spec  : CompilerSpec.t
  ; in_path  : string
  ; out_path : string
  }

let compile_gcc (cmp : compilation) : unit Or_error.t =
  let final_argv =
  [ "-S"       (* emit assembly *)
  ; "-fno-pic" (* don't emit position-independent code *)
  ]
  @ cmp.cc_spec.argv
  @ [ "-o"
    ; cmp.out_path
    ; cmp.in_path
    ] in
  Run.run ~prog:cmp.cc_spec.cmd final_argv

let compile (cc_id : string) (cc_spec : CompilerSpec.t) (ps : Pathset.t) =
  let asm_path = List.Assoc.find_exn ps.a_paths cc_id ~equal:(=) in
  let cmp = { cc_id = cc_id
            ; cc_spec = cc_spec
            ; in_path = ps.c_path
            ; out_path = asm_path
            } in
  match cc_spec.style with
  | Gcc -> compile_gcc cmp

let test (cc_spec : CompilerSpec.t) =
  match cc_spec.style with
  | Gcc -> Run.run ~prog:cc_spec.cmd ["--version"]
