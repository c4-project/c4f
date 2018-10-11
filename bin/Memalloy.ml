(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Lib
open Utils
open Utils.MyContainers

let c_asm o (cid : CompilerSpec.Id.t) (spec : CompilerSpec.t) (ps : Pathset.t) =
  let open Io in
  let f src inp _ outp =
    let iname = MyFormat.format_to_string (In_source.pp) src in
    Litmusifier.run
      { o
      ; cid
      ; spec
      ; iname
      ; inp
      ; outp
      ; mode = `Litmusify
      }
  in
  Or_error.tag ~tag:"While translating assembly to litmus"
    (with_input_and_output
     (`File (Pathset.compiler_asm_path ps cid))
     (`File (Pathset.compiler_lita_path ps cid))
     ~f)

let proc_c (o : OutputCtx.t) specs ~in_root ~out_root c_fname =
  let open Or_error.Let_syntax in
  let%bind paths =
    Pathset.make_and_mkdirs specs ~out_root ~in_root ~c_fname
  in
  Pathset.pp o.vf paths;
  Format.pp_print_newline o.vf ();
  MyList.iter_result
    (fun (cid, cs) ->
       Format.fprintf o.vf "@[CC[%a]@ %s@]@."
         CompilerSpec.Id.pp cid
         paths.basename;
       Compiler.compile cid cs paths
       >>= (fun () -> c_asm o cid cs paths)
    ) specs

let is_c_file = MyFilename.has_extension ~ext:"c"

let get_c_files (c_path : string) : string array Or_error.t =
  Or_error.(
    tag_arg
      (try_with (fun () -> Sys.readdir c_path))
      "Couldn't open directory: is this definitely a memalloy run?"
      c_path
      [%sexp_of: string]
    >>| Array.filter ~f:is_c_file
  )

let check_c_files_exist c_path c_files =
    if Array.is_empty c_files
    then
      Or_error.error
        "Expected at least one C file." c_path [%sexp_of:string]
    else Result.ok_unit

let run o specs ~in_root ~out_root =
  let open Or_error.Let_syntax in
  let c_path = Filename.concat in_root "C" in
  let%bind c_files = get_c_files c_path in
  let%bind _ = check_c_files_exist c_path c_files in
  MyArray.iter_result (proc_c o specs ~in_root ~out_root) c_files
