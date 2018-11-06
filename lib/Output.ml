(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core
open Utils

type t =
  { vf : Format.formatter
  ; wf : Format.formatter
  ; ef : Format.formatter
  }

let maybe_err_formatter on =
  if on
  then Format.err_formatter
  else My_format.null_formatter ()
;;

let make ~verbose ~warnings =
  { vf = maybe_err_formatter verbose
  ; wf = maybe_err_formatter warnings
  ; ef = Format.err_formatter
  }
;;

let log_stage o ~stage ~file compiler_id =
  Format.fprintf o.vf "@[%s[%a]@ %s@]@."
    stage
    Spec.Id.pp compiler_id
    file
;;

let print_error o =
  Result.iter_error
    ~f:(Format.fprintf o.ef
          "@[act encountered a top-level error:@.@[%a@]@]@." Error.pp)
;;
