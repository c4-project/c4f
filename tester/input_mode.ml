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
open Utils

type t =
  | Memalloy of Fpath.t
  | Litmus_only of Fpath.t list
[@@deriving variants]

let reduce (imode : t)
           ~(memalloy : Fpath.t -> 'a)
           ~(litmus_only : Fpath.t list -> 'a)
    : 'a =
  Variants.map imode ~memalloy:(fun _ -> memalloy) ~litmus_only:(fun _ -> litmus_only)
;;

let must_delitmusify : t -> bool =
  reduce ~memalloy:(Fn.const false) ~litmus_only:(Fn.const true)
;;

(** These bits depend on filesystem operations; to make them more
    easily testable, we parametrise out said operations, then
    include the version based on the real ('Unix') filesystem. *)
module With_fs (F : Fs.S) = struct
  let validate_input_root_exists : Fpath.t Validate.check =
    Validate.of_error F.check_is_dir
  ;;

  let validate_file_exists : Fpath.t Validate.check = Validate.of_error F.check_is_file

  let validate_files_exist : Fpath.t list Validate.check =
    Validate.list ~name:Fpath.to_string validate_file_exists
  ;;

  let validate : t Validate.check =
    reduce ~memalloy:validate_input_root_exists ~litmus_only:validate_files_exist
  ;;

  let memalloy ~(input_root : Fpath.t) : t Or_error.t =
    Validate.valid_or_error (Memalloy input_root) validate
  ;;

  let litmus_only ~(files : Fpath.t list) : t Or_error.t =
    Validate.valid_or_error (Litmus_only files) validate
  ;;
end

include With_fs (Fs.Unix)
