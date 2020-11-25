(** * Mechanised safe 32-bit operations.

    These proofs can probably be expanded to arbitrary bit depths, but may
    require the replacement of [lia] with some more abstract arithmetic. *)

Require Import
        Coq.ZArith.BinInt
        Coq.Program.Utils
        Lia
.

Open Scope program_scope.
Open Scope Z_scope.

Definition imin : Z := (-2147483648)%Z.
Definition imax : Z := (2147483647)%Z.

(** On twos-complement systems, we can define the minimum in terms of the maximum. *)
Lemma imin_imax : imin = (-imax) - 1.
Proof.
  trivial.
Qed.

(** An integer clamped by proof to be between [imin] and [imax]. *)
Definition int32 := { x : Z | imin <= x <= imax }.

(** Simpler shorthand for constructing an [int32]. *)
Definition mki32 (x : Z) (Px : imin <= x <= imax) : int32 := exist _ x Px.

Definition add_imin (x : int32) (Pn : 0 <= `x) : int32.
Proof.
  refine (mki32 (imin + `x) _).
  destruct x; unfold proj1_sig in *; rewrite imin_imax; lia.
Defined.

(** Subtracting a non-positive number from [imin]. *)
Definition sub_imin (x : int32) (Pn : `x <= 0) : int32.
Proof.
  refine (mki32 (imin - `x) _).
  destruct x as (x & Px1 & Px2); unfold proj1_sig, imin, imax in *.
  lia.
Defined.

(** Adding a non-positive number to [imax]. *)
Definition add_imax (x : int32) (Pn : `x <= 0) : int32.
Proof.
  refine (mki32 (imax + `x) _).
  destruct x as (x & Px1 & Px2).
  unfold proj1_sig, imin, imax in *.
  lia.
Defined.

(** Subtracting a non-negative number from [imax]. *)
Definition sub_imax (x : int32) (Pn : 0 <= `x) : int32.
Proof.
  refine (mki32 (imax - `x) _).
  destruct x as (x & Px1 & Px2).
  unfold proj1_sig, imin, imax in *.
  lia.
Defined.

Lemma add_imin_unfold (x : int32) (P : 0 <= `x) : proj1_sig (add_imin x P) = imin + (`x).
Proof.
  trivial.
Qed.

Lemma sub_imin_unfold (x : int32) (P : `x <= 0) : proj1_sig (sub_imin x P) = imin - (`x).
Proof.
  trivial.
Qed.

Lemma add_imax_unfold (x : int32) (P : `x <= 0) : proj1_sig (add_imax x P) = imax + (`x).
Proof.
  trivial.
Qed.

Lemma sub_imax_unfold (x : int32) (P : 0 <= `x) : proj1_sig (sub_imax x P) = imax - (`x).
Proof.
  trivial.
Qed.

(** If int32 [x] is non-negative, the difference between it and any [y] cannot underflow. *)
Lemma sub32_nonneg_no_underflow_l (x y : int32) : 0 <= `x -> imin <= (`x - `y).
Proof.
  destruct x, y; simpl; rewrite imin_imax; lia.
Qed.

(** If int32 [y] is non-positive, the difference between [x] and any it cannot underflow. *)
Lemma sub32_nonpos_no_underflow_r (x y : int32) : `y <= 0 -> imin <= (`x - `y).
Proof.
  destruct x; simpl; lia.
Qed.

(** If int32 [x] is negative, the difference between it and [y] cannot overflow. *)
Lemma sub32_neg_no_overflow_l (x y : int32) : `x < 0 -> (`x - `y) <= imax.
Proof.
  destruct y; simpl; rewrite imin_imax in *; lia.
Qed.

(** If int32 [y] is non-negative, the difference between [x] and it cannot overflow. *)
Lemma sub32_nonneg_no_overflow_r (x y : int32) : 0 <= `y -> (`x - `y) <= imax.
Proof.
  destruct x; simpl; lia.
Qed.

(** If int32 [x] is non-negative, adding it to [y] cannot underflow. *)
Lemma add32_nonneg_no_underflow_l (x y : int32) : 0 <= `x -> imin <= `x + `y.
Proof.
  destruct y; simpl; lia.
Qed.

(** If int32 [y] is non-negative, adding [x] to it cannot underflow. *)
Corollary add32_nonneg_no_underflow_r (x y : int32) : 0 <= `y -> imin <= `x + `y.
Proof.
  rewrite Z.add_comm; apply add32_nonneg_no_underflow_l.
Qed.

(** If int32 [x] is non-positive, adding it to [y] cannot overflow. *)
Lemma add32_nonpos_no_overflow_l (x y : int32) : `x <= 0 -> `x + `y <= imax.
Proof.
  destruct x as (x & Hx), y as (y & Hy); simpl; lia.
Qed.

(** If int32 [y] is non-positive, adding [x] to it cannot underflow. *)
Corollary add32_nonpos_no_overflow_r (x y : int32) : `y <= 0 -> `x + `y <= imax.
Proof.
  rewrite Z.add_comm; apply add32_nonpos_no_overflow_l.
Qed.

(** We can express subtraction underflow checking in an under/overflow-free manner.
    Note that if y = 0, we have a free choice of whether to do the check or not. *)
Lemma sub32_under (x y : int32) :
  { P : 0 <= `y | (proj1_sig (add_imin y P)) <= `x } \/ `y <= 0 <-> imin <= (`x - `y).
Proof.
  split.
  - intros [(a & Ha) | Hyz].
    + now rewrite <- Z.le_add_le_sub_r.
    + destruct x, y; unfold proj1_sig, imin, imax in *; lia.
  - intros Hoc%Z.le_add_le_sub_r.
    destruct (ZArith_dec.Z_gt_le_dec 0 (`y)) as [Hyz|Hzy].
    + right.
      now apply Z.lt_le_incl, Z.gt_lt.
    + left.
      now exists Hzy.
Qed.

(** We can express subtraction overflow checking in an under/overflow-free manner.
    Note that if y = 0, we have a free choice of whether to do the check or not. *)
Lemma sub32_over (x y : int32) :
  { P : `y <= 0 | `x <= (proj1_sig (add_imax y P)) } \/ 0 <= `y <-> (`x - `y) <= imax.
Proof.
  split.
  - intros [(a & Ha) | Hzy].
    + now rewrite Z.le_sub_le_add_r.
    + destruct x, y; unfold proj1_sig, imin, imax in *; lia.
  - intros Hoc%Z.le_sub_le_add_r.
    destruct (ZArith_dec.Z_gt_le_dec (`y) 0) as [Hzy|Hyz].
    + right.
      now apply Z.lt_le_incl, Z.gt_lt.
    + left.
      now exists Hyz.
Qed.

(** Safe arbitrary subtraction, given computed overflow and underflow checks. *)
Definition sub32
           (x y : int32)
           (Hunder : { P : 0 <= `y | (proj1_sig (add_imin y P)) <= `x } \/ (`y <= 0))
           (Hover  : { P : `y <= 0 | `x <= (proj1_sig (add_imax y P)) } \/ (0 <= `y))
 : int32.
Proof.
  refine (mki32 (`x - `y) _).
  split.
  - now apply sub32_under.
  - now apply sub32_over.
Defined.

(** We can express addition underflow checking in an under/overflow-free manner.
    Note that if y = 0, we have a free choice of whether to do the check or not;
    also note that commutativity means we can flip to overflow checking on [x]. *)
Lemma add32_under (x y : int32) :
 { P : `y <= 0 | (proj1_sig (sub_imin y P)) <= `x } \/ 0 <= `y <-> imin <= (`x + `y).
Proof.
  split.
  - intros [(a & Ha) | Hyz].
    + now rewrite <- Z.le_sub_le_add_r.
    + destruct x, y; unfold proj1_sig, imin, imax in *; lia.
  - intros Hoc%Z.le_sub_le_add_r.
    destruct (ZArith_dec.Z_gt_le_dec 0 (`y)) as [Hyz|Hzy]; intuition.
    left.
    apply Z.gt_lt, Z.lt_le_incl in Hyz.
    now exists Hyz.
Qed.

(** We can express addition overflow checking in an under/overflow-free manner.
    Note that if y = 0, we have a free choice of whether to do the check or not;
    also note that commutativity means we can flip to overflow checking on [x]. *)
Lemma add32_over (x y : int32) :
  { P : 0 <= `y | `x <= (proj1_sig (sub_imax y P)) } \/ `y <= 0 <-> (`x + `y) <= imax.
Proof.
  split.
  - intros [(a & Ha) | Hzy].
    + now rewrite Z.le_add_le_sub_r.
    + destruct x, y; unfold proj1_sig, imin, imax in *; lia.
  - intros Hoc%Z.le_add_le_sub_r.
    destruct (ZArith_dec.Z_gt_le_dec (`y) 0) as [Hzy|Hyz]; intuition.
    left.
    apply Z.gt_lt, Z.lt_le_incl in Hzy.
    now exists Hzy.
Qed.

(** Safe arbitrary addition, given computed overflow and underflow checks. *)
Definition add32
           (x y : int32)
           (Hunder : { P : `y <= 0 | (proj1_sig (sub_imin y P)) <= `x } \/ 0 <= `y)
           (Hover  : { P : 0 <= `y | `x <= (proj1_sig (sub_imax y P)) } \/ `y <= 0)
 : int32.
Proof.
  refine (mki32 (`x + `y) _).
  split.
  - now apply add32_under.
  - now apply add32_over.
Defined.

(** We can forget that a subtraction doesn't underflow or overflow. *)
Lemma sub32_sub (x y : int32) Hunder Hover : proj1_sig (sub32 x y Hunder Hover) = `x - `y.
Proof.
  trivial.
Qed.

(** We can forget that an addition doesn't underflow or overflow. *)
Lemma add32_add (x y : int32) Hunder Hover : proj1_sig (add32 x y Hunder Hover) = `x + `y.
Proof.
  trivial.
Qed.

(* WORK IN PROGRESS

Program Definition zero32 : int32 := mki32 0 _.
Next Obligation.
  unfold imin, imax; lia.
Defined.

Program Definition imin32 : int32 := mki32 imin _.
Next Obligation.
  unfold imin, imax; lia.
Defined.

Program Definition imax32 : int32 := mki32 imax _.
Next Obligation.
  unfold imin, imax; lia.
Defined.

Definition sub32_compute (x y : int32) : (int32 * int32).
Proof.
  refine (
      match Z.compare (`y) 0 as yz, Z.compare (`x) 0 as xz return Z.compare (`y) 0 = yz -> Z.compare (`x) 0 = xz -> _ with
      | Eq, _ => fun _ _ => (zero32, x)
      | Lt, Lt | Gt, Eq | Gt, Gt => fun Hyzl Hxzl => (zero32, sub32 x y _ _)
      | Lt, Eq | Lt, Gt => fun Hyzl Hxzl => (* Overflow *) (imax32, sub32 (add32 x imax32 _ _) y _ _)
      | Gt, Lt => fun Hyzl Hxzl => (* Underflow *) (imin32, sub32 (add32 x imin32 _ _) y _ _)
      end _ _
    ).
  - destruct x as (x & Hx), y as (y & Hy); unfold proj1_sig in *.
    apply Z.compare_eq in Hxzl.
    subst.
    right.
    now left.
  - apply Z.compare_eq in Hxzl.
    apply -> Z.compare_lt_iff in Hyzl.
    left.
    apply Z.lt_le_incl in Hyzl.
    apply exist with (x := Hyzl).
    rewrite add_imax_unfold.
    destruct x, y; unfold proj1_sig, imax, imin in *; subst.
                            lia.

    
*)
