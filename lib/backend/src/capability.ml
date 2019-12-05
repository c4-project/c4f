(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Flag = struct
  module M = struct
    type t =
      | Run  (** This backend can run tests directly. *)
      | Make_harness
          (** This backend can produce a harness that can be compiled and run
              to *)
    [@@deriving enum, sexp, compare, equal]

    let table : (t, string) List.Assoc.t =
      [(Run, "run"); (Make_harness, "make-harness")]
  end

  include M
  include Comparable.Make (M)
  include Act_utils.Enum.Extend_table (M)
end

module Summary = struct
  type t = {flags: Set.M(Flag).t; arches: Set.M(Arch).t}
  [@@deriving fields, make]
end

module Run = struct
  type t =
    | Cannot_run of {why: Error.t}
    | Can_run of {argv_f: input_file:string -> string list Or_error.t}
end

module Make_harness = struct
  type t =
    | Cannot_make_harness of {why: Error.t}
    | Can_make_harness of
        { argv_f:
            input_file:string -> output_dir:string -> string list Or_error.t
        ; run_as: string list }
end
