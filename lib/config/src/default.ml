(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

module Parametric (C : Sexpable.S) = struct
  type t = {arches: C.t; compilers: C.t; machines: C.t; sims: C.t}
  [@@deriving sexp, fields]
end

module M = Parametric (struct
  type t = Ac.Id.t list [@@deriving sexp]
end)

include M

module Build = struct
  include Parametric (struct
    type t = Ac.Id.t Queue.t [@@deriving sexp]
  end)

  let add_from_ast (ds : t) : Ast.Default.t -> unit = function
    | Try (Arch, x) ->
        Queue.enqueue (arches ds) x
    | Try (Compiler, x) ->
        Queue.enqueue (compilers ds) x
    | Try (Machine, x) ->
        Queue.enqueue (machines ds) x
    | Try (Sim, x) ->
        Queue.enqueue (sims ds) x

  let listify (ds : t) : M.t =
    M.Fields.create
      ~arches:(Queue.to_list (arches ds))
      ~compilers:(Queue.to_list (compilers ds))
      ~machines:(Queue.to_list (machines ds))
      ~sims:(Queue.to_list (sims ds))

  let of_ast (nodes : Ast.Default.t list) : M.t =
    let ds_queues =
      Fields.create ~arches:(Queue.create ()) ~compilers:(Queue.create ())
        ~machines:(Queue.create ()) ~sims:(Queue.create ())
    in
    List.iter nodes ~f:(add_from_ast ds_queues) ;
    listify ds_queues
end

let make ?(arches : Ac.Id.t list = []) ?(compilers : Ac.Id.t list = [])
    ?(machines : Ac.Id.t list = []) ?(sims : Ac.Id.t list = []) () : t =
  Fields.create ~arches ~compilers ~machines ~sims

let of_ast (ast : Ast.Default.t list) : t Or_error.t =
  Or_error.return (Build.of_ast ast)

let bucket_to_ast (category : Ast.Default.Category.t) :
    Ac.Id.t list -> Ast.Default.t list =
  List.map ~f:(fun id -> Ast.Default.Try (category, id))

let to_ast (ds : t) : Ast.Default.t list =
  List.concat
    [ bucket_to_ast Arch (arches ds)
    ; bucket_to_ast Compiler (compilers ds)
    ; bucket_to_ast Machine (machines ds)
    ; bucket_to_ast Sim (sims ds) ]
