(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base

type t =
  { output_root : Fpath.t
  ; compilers : Config.Id.Set.t
  ; input_mode : Input_mode.t
  }
[@@deriving fields]

let validate_directory : Fpath.t Validate.check =
  Validate.booltest Fpath.is_dir_path ~if_false:"Expected a local directory here."
;;

let validate (cfg : t) : Validate.t =
  let module V = Validate in
  let w check = V.field_folder cfg check in
  V.of_list
    (Fields.fold
       ~init:[]
       ~output_root:(w validate_directory)
       ~compilers:(w (Fn.const V.pass))
       ~input_mode:(w (Fn.const V.pass)))
;;

let make
    ~(output_root : Fpath.t)
    ~(compilers : Config.Id.Set.t)
    ~(input_mode : Input_mode.t)
    : t Or_error.t
  =
  let cfg = Fields.create ~output_root ~compilers ~input_mode in
  Validate.valid_or_error cfg validate
;;
