(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module C = Act_c

module Rewriter_with_thread (T : Thread.S) = struct
  (** [address_globals stm] converts each address in [stm] over a global
      variable [v] to [&*v], ready for {{!ref_globals} ref_globals} to
      reduce to [&v]. *)
  let address_globals : C.Mini.Statement.t -> C.Mini.Statement.t =
    C.Mini.Statement.On_addresses.map
      ~f:
        (T.when_global ~over:C.Mini.Address.variable_of ~f:(fun addr ->
             (* The added deref here will be removed in [ref_globals]. *)
             C.Mini.Address.ref
               (C.Mini.Address.On_lvalues.map ~f:C.Mini.Lvalue.deref addr)
         ))

  (** [ref_globals stm] turns all dereferences of global variables in [stm]
      into direct accesses to the same variables. *)
  let ref_globals : C.Mini.Statement.t -> C.Mini.Statement.t =
    C.Mini.Statement.On_lvalues.map
      ~f:
        C.Mini.Lvalue.(
          T.when_global ~over:variable_of
            ~f:(Fn.compose variable variable_of))

  let proc_stm (stm : C.Mini.Statement.t) : C.Mini.Statement.t =
    stm |> address_globals |> ref_globals
    |> Qualify.locals_in_statement (module T)

  let rewrite_statements : C.Mini.Statement.t list -> C.Mini.Statement.t list =
    List.map ~f:proc_stm

  let populate_parameters (_func) = []

  let rewrite (func : C.Mini.Function.t) =
    C.Mini.Function.map func ~parameters:(fun _ -> populate_parameters func)
      ~body_decls:(Fn.const [])
      ~body_stms:rewrite_statements
end

let rewrite (tid : int) (func : C.Mini.Function.t) :
    C.Mini.Function.t =
  let module T = Thread.Make (struct
    let tid = tid

    let locals =
      func |> C.Mini.Function.body_decls |> List.map ~f:fst |> C.Mini.Identifier.Set.of_list
  end) in
  let module M = Rewriter_with_thread (T) in M.rewrite func

let rewrite_all :
       C.Mini.Function.t C.Mini_intf.id_assoc
    -> C.Mini.Function.t C.Mini_intf.id_assoc =
  List.mapi ~f:(fun tid (name, f) -> (name, rewrite tid f))


