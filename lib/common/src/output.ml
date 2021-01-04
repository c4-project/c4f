(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Au = Act_utils
end

type t = {verbose: bool; vf: Formatter.t; wf: Formatter.t; ef: Formatter.t}

let maybe_err_formatter (on : bool) : Formatter.t =
  if on then Fmt.stderr else Au.My_format.null_formatter ()

let make ~(verbose : bool) ~(warnings : bool) : t =
  { verbose
  ; vf= maybe_err_formatter verbose
  ; wf= maybe_err_formatter warnings
  ; ef= Fmt.stderr }

let is_verbose (x : t) : bool = x.verbose

let silent () : t =
  let nullf = Au.My_format.null_formatter () in
  {verbose= false; vf= nullf; wf= nullf; ef= nullf}

let pv (type a) (o : t) : (a, Formatter.t, unit) format -> a = Fmt.pf o.vf

let pw (type a) (o : t) : (a, Formatter.t, unit) format -> a = Fmt.pf o.wf

let pe (type a) (o : t) : (a, Formatter.t, unit) format -> a = Fmt.pf o.ef

let print_error_body : Error.t Fmt.t =
  Fmt.(
    vbox ~indent:2
      ( hbox (styled (`Fg `Red) (any "ACT encountered a top-level error:@ "))
      ++ box Error.pp ))

let print_error (o : t) : 'a Or_error.t -> unit =
  Fmt.(result ~ok:nop ~error:(print_error_body ++ any "@.")) o.ef
