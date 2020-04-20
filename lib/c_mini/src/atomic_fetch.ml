(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type 'e t = {obj: Address.t; arg: 'e; mo: Mem_order.t; op: Op.Fetch.t}
[@@deriving sexp, fields, make, compare, equal, quickcheck]

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : 'a t) ~(obj : Address.t -> Address.t Ap.t)
      ~(arg : 'a -> 'b Ap.t) ~(mo : Mem_order.t -> Mem_order.t Ap.t)
      ~(op : Op.Fetch.t -> Op.Fetch.t Ap.t) : 'b t Ap.t =
    Ap.(
      return (fun obj arg mo op -> make ~obj ~arg ~mo ~op)
      <*> obj x.obj <*> arg x.arg <*> mo x.mo <*> op x.op)
end

module On_expressions : Travesty.Traversable_types.S1 with type 'e t = 'e t =
Travesty.Traversable.Make1 (struct
  type nonrec 'e t = 'e t

  module On_monad (M : Monad.S) = struct
    module B = Base_map (struct
      type 'a t = 'a M.t

      include Applicative.Of_monad (M)
    end)

    let map_m (x : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
      B.bmap x ~obj:M.return ~arg:f ~mo:M.return ~op:M.return
  end
end)

module Type_check (Env : Env_types.S) = struct
  type nonrec t = Type.t t

  module Ad = Address.Type_check (Env)

  let check_arg_obj ~(arg : Type.t) ~(obj : Type.t) : Type.t Or_error.t =
    Or_error.tag
      (Type.check_atomic_non ~atomic:arg ~non:obj)
      ~tag:"'obj' type must be atomic version of 'expected' type"

  let type_of (c : t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      (* A* *)
      let%bind obj = Ad.type_of (obj c) in
      (* M *)
      let arg = arg c in
      (* C11 allows A to be a pointer type if M is ptrdiff_t; we don't
         implement double-pointer types and so don't implement this (yet). *)
      let%map _ = check_arg_obj ~arg ~obj in
      (* As we don't implement M=ptrdiff_t, M = C and is thus the return
         type. *)
      arg)
end
