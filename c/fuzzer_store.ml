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

(* Module shorthands *)
module Action  = Fuzzer_action
module Subject = Fuzzer_subject
module State   = Fuzzer_state
module Var     = Fuzzer_var

module Int : Action.S = struct
  (** Lists the restrictions we put on destination variables. *)
  let src_restrictions : (Var.Record.t -> bool) list Lazy.t =
    lazy []
  ;;

  (** Lists the restrictions we put on destination variables. *)
  let dst_restrictions : (Var.Record.t -> bool) list Lazy.t =
    lazy
      Var.Record.[ was_generated
                   (* This is to make sure that we don't change the
                      observable semantics of the program over its
                      original variables. *)
                 ; has_no_dependencies
                   (* This action changes the value, so we can't do it
                      to variables with depended-upon values. *)
                 ]
  ;;

  module Random_state = struct
    type t =
      { store : Mini.Atomic_store.t
      ; path  : Mini_path.stm_hole Mini_path.program_path
      }

    module G = Quickcheck.Generator

    (* TODO(@MattWindsor91): move this to Atomic_store. *)

    let gen_src (vars : Var.Map.t) : Mini.Expression.t G.t =
      let predicates = Lazy.force src_restrictions in
      let env = Var.Map.env_satisfying_all ~predicates vars in
      let module E = Mini_env.Make (struct let env = env end) in
      let module Q = Mini.Expression.Quickcheck_int_values (E) in
      Q.gen
    ;;

    let gen_dst (vars : Var.Map.t) : Mini.Address.t G.t =
      let predicates = Lazy.force dst_restrictions in
      let env = Var.Map.env_satisfying_all ~predicates vars in
      let module E = Mini_env.Make (struct let env = env end) in
      let module Q = Mini.Address.Quickcheck_atomic_int_pointers (E) in
      Q.gen

    let gen_store (vars : Var.Map.t) : Mini.Atomic_store.t G.t =
      let open G.Let_syntax in
      let%bind src = gen_src vars in
      let%bind dst = gen_dst vars in
      let%map  mo  = Mem_order.gen_store in
      Mini.Atomic_store.make ~src ~dst ~mo

    let gen' (subject : Subject.Test.t) (vars : Var.Map.t) : t G.t =
      let open G.Let_syntax in
      let%bind store = gen_store vars in
      let%map  path = Subject.Test.Path.gen_insert_stm subject in
      { store; path }
    ;;

    let gen (subject : Subject.Test.t) : t G.t State.Monad.t =
      State.Monad.with_vars (gen' subject)
    ;;
  end

  let available _ =
    State.Monad.with_vars (
      Var.Map.exists_satisfying_all
        ~predicates:(Lazy.force dst_restrictions)
    )
  ;;

  (* This action writes to the destination, so we no longer have a
     known value for it. *)
  let erase_value_of_store_dst (store : Mini.Atomic_store.t)
    : unit State.Monad.t =
    let dst = Mini.Atomic_store.dst store in
    let dst_var = Mini.Address.variable_of dst in
    State.Monad.erase_var_value dst_var
  ;;

  module Exp_idents =
    Mini.Expression.On_identifiers.On_monad (State.Monad)
  ;;

  (* This action also introduces dependencies on every variable in
     the source. *)
  let add_dependencies_to_store_src (store : Mini.Atomic_store.t)
    : unit State.Monad.t =
    Exp_idents.iter_m (Mini.Atomic_store.src store)
      ~f:State.Monad.add_dependency
  ;;

  let run (subject : Subject.Test.t)
      ( { store; path } : Random_state.t)
    : Subject.Test.t State.Monad.t =
    let open State.Monad.Let_syntax in
    let store_stm = Mini.Statement.atomic_store store in
    let%bind () = erase_value_of_store_dst store in
    let%bind () = add_dependencies_to_store_src store in
    State.Monad.Monadic.return (
      Subject.Test.Path.insert_stm path store_stm subject
    )
  ;;
end
