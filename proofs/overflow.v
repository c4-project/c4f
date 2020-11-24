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

(** Safe arbitrary subtraction, given computed overflow and underflow checks. *)
Definition sub32
           (x y : int32)
           (Hunder : { P : 0 <= `y | (proj1_sig (add_imin y P)) <= `x } + (0 <= `x \/ `y <= 0))
           (Hover  : { P : `y <= 0 | `x <= (proj1_sig (add_imax y P)) } + (0 <= `y \/ `x < 0))
 : int32.
Proof.
  refine (mki32 (`x - `y) _).
  split.
  - destruct Hunder as [(Hzly & Hylx)|[Hxp|Hyn]].
    + rewrite <- Z.le_add_le_sub_r.
      destruct (add_imin y Hzly) as (a & Ha) eqn:Hae.
      destruct x as (x & Hx), y as (y & Hy).
      apply proj1_sig_eq in Hae.
      rewrite add_imin_unfold in Hae.
      unfold proj1_sig in *.
      now subst.
    + now apply sub32_nonneg_no_underflow_l.
    + now apply sub32_nonpos_no_underflow_r.
  - destruct Hover as [(Hylz & Hxly)|[Hyp|Hxn]].
    + rewrite Z.le_sub_le_add_r.
      destruct (add_imax y Hylz) as (a & Ha) eqn:Hae.
      destruct x as (x & Hx), y as (y & Hy).
      apply proj1_sig_eq in Hae.
      rewrite add_imax_unfold in Hae.
      unfold proj1_sig in *.
      now subst.
    + now apply sub32_nonneg_no_overflow_r.
    + now apply sub32_neg_no_overflow_l.
Defined.

(** Safe arbitrary addition, given computed overflow and underflow checks. *)
Definition add32
           (x y : int32)
           (Hunder : { P : `y <= 0 | (proj1_sig (sub_imin y P)) <= `x } + (0 <= `x \/ 0 <= `y))
           (Hover  : { P : 0 <= `y | `x <= (proj1_sig (sub_imax y P)) } + (`x <= 0 \/ `y <= 0))
 : int32.
Proof.
  refine (mki32 (`x + `y) _).
  split.
  - destruct Hunder as [(Hylz & Hylx)|[Hxp|Hyn]].
    + rewrite <- Z.le_sub_le_add_r.
      destruct (sub_imin y Hylz) as (a & Ha) eqn:Hae.
      destruct x as (x & Hx), y as (y & Hy).
      apply proj1_sig_eq in Hae.
      rewrite sub_imin_unfold in Hae.
      unfold proj1_sig in *.
      now subst.
    + now apply add32_nonneg_no_underflow_l.
    + now apply add32_nonneg_no_underflow_r.
  - destruct Hover as [(Hzly & Hxly)|[Hyp|Hxn]].
    + rewrite Z.le_add_le_sub_r.
      destruct (sub_imax y Hzly) as (a & Ha) eqn:Hae.
      destruct x as (x & Hx), y as (y & Hy).
      apply proj1_sig_eq in Hae.
      rewrite sub_imax_unfold in Hae.
      unfold proj1_sig in *.
      now subst.
    + now apply add32_nonpos_no_overflow_l.
    + now apply add32_nonpos_no_overflow_r.
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

