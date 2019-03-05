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

open Core_kernel

module C_litmus_mode = struct
  type t =
    | Memalloy
    | Delitmusify
end

type t =
  { fnames        : string list
  ; in_root       : Fpath.t
  ; out_root      : Fpath.t
  ; compilers     : Id.Set.t
  ; c_litmus_mode : C_litmus_mode.t
  }
[@@deriving fields]
;;

let validate_name : string Validate.check =
  let module V = Validate in
  V.all
    [ V.booltest (Fn.non (String.is_substring ~substring:Fpath.dir_sep))
        ~if_false:"Filename shouldn't contain directories."
    ; V.booltest (Fn.non (fun s -> String.contains s '.'))
        ~if_false:"Filename shouldn't contain an extension."
    ]
;;

let validate_directory : Fpath.t Validate.check =
  Validate.booltest Fpath.is_dir_path
    ~if_false:"Expected a local directory here."
;;

let validate (cfg : t) : Validate.t =
  let module V = Validate in
  let w check = V.field_folder cfg check in
  V.of_list
    (Fields.fold ~init:[]
       ~fnames:(w (V.list ~name:Fn.id validate_name))
       ~in_root:(w validate_directory)
       ~out_root:(w validate_directory)
       ~compilers:(w (Fn.const V.pass))
       ~c_litmus_mode:(w (Fn.const V.pass))
    )
;;

let make
    ~(fnames        : string list)
    ~(in_root       : Fpath.t)
    ~(out_root      : Fpath.t)
    ~(compilers     : Id.Set.t)
    ?(c_litmus_mode : C_litmus_mode.t = C_litmus_mode.Memalloy)
    ()
  : t Or_error.t =
  let cfg =
    Fields.create
      ~fnames ~in_root ~out_root ~compilers
      ~c_litmus_mode
  in
  Validate.valid_or_error cfg validate
;;
