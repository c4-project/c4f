(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type 'e t =
  { obj: Address.t
  ; expected: Address.t
  ; desired: 'e
  ; succ: Mem_order.t
  ; fail: Mem_order.t [@quickcheck.generator Mem_order.gen_cmpxchg_fail] }
[@@deriving sexp, fields, make, compare, equal, quickcheck]

(* The 'fail' memory order cannot be stronger than the 'succ' order. (At time
   of writing, quickcheck shrinkers for mem-orders are atomic, and so we
   shan't need to override the shrinker) *)

let quickcheck_generator e =
  Base_quickcheck.Generator.filter (quickcheck_generator e) ~f:(fun x ->
      Mem_order.(x.fail <= x.succ) )

let ensure_mo_compat (old : 'a t) (succ : Mem_order.t) (fail : Mem_order.t) :
    Mem_order.t * Mem_order.t =
  if Mem_order.(is_cmpxchg_fail_compatible fail && fail <= succ) then
    (succ, fail)
  else (old.succ, old.fail)

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : 'a t) ~(obj : Address.t -> Address.t Ap.t)
      ~(expected : Address.t -> Address.t Ap.t) ~(desired : 'a -> 'b Ap.t)
      ~(succ : Mem_order.t -> Mem_order.t Ap.t)
      ~(fail : Mem_order.t -> Mem_order.t Ap.t) : 'b t Ap.t =
    Ap.(
      let m obj expected desired succ fail =
        let succ, fail = ensure_mo_compat x succ fail in
        make ~obj ~expected ~desired ~succ ~fail
      in
      return m <*> obj x.obj <*> expected x.expected <*> desired x.desired
      <*> succ x.succ <*> fail x.fail)
end

module On_expressions : Travesty.Traversable_types.S1 with type 'e t = 'e t =
Travesty.Traversable.Make1 (struct
  type nonrec 'e t = 'e t

  module On_monad (M : Monad.S) = struct
    module B = Base_map (Act_utils.Applicative.Of_monad_ext (M))

    let map_m (x : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
      B.bmap x ~obj:M.return ~expected:M.return ~desired:f ~succ:M.return
        ~fail:M.return
  end
end)

module Type_check (Env : Env_types.S) = struct
  type nonrec t = Type.t t

  module Ad = Address.Type_check (Env)

  let check_expected_desired ~(expected : Type.t) ~(desired : Type.t) :
      Type.t Or_error.t =
    Or_error.tag
      (Type.check_pointer_non ~pointer:expected ~non:desired)
      ~tag:"'expected' type must be same as pointer to 'desired' type"

  let check_expected_obj ~(expected : Type.t) ~(obj : Type.t) :
      Type.t Or_error.t =
    Or_error.tag
      (Type.check_atomic_non ~atomic:expected ~non:obj)
      ~tag:"'obj' type must be atomic version of 'expected' type"

  let type_of (c : t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      (* A* *)
      let%bind obj = Ad.type_of (obj c) in
      (* C* *)
      let%bind expected = Ad.type_of (expected c) in
      (* C *)
      let desired = desired c in
      let%bind _ = check_expected_desired ~expected ~desired in
      let%map _ = check_expected_obj ~expected ~obj in
      (* Compare-exchanges return a boolean: whether or not they were
         successful. *)
      Type.bool ())
end
