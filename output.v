Require Import Omega.

Theorem A: forall a b,(((0 < b) /\ (b < a)) -> forall x1:nat, forall x0:nat, ((((x0 < x1) /\ ((a = ((mult b x0) + x1)) /\ ((0 < b) /\ (b < x1)))) -> ((a = ((mult b (x0 + 1)) + (x1 - b))) /\ ((0 < b) /\ (b < (x1 - b))))) /\ (((not (x0 < x1)) /\ ((a = ((mult b x0) + x1)) /\ ((0 < b) /\ (b < x1)))) -> (a = ((mult b x0) + x1))))).
Proof.
  intros a b [H0 H1] x0 x1.
  split.
  split.
  destruct H as [H2 [H3 H4]].
Admitted.

