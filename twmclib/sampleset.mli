(** The type of saturated sets of samples. *)
type t

(** Pretty-printing with {b PPrint}. *)
val pp : t -> PPrint.document

(** The empty saturated set of samples. *)
val empty : t

(** [saturate a s] takes a sample [a] and a saturated sample set [s] and returns
   the smallest saturated sample set larger than [s] and containing [a]. *)
val saturate : Sample.t -> t -> t

(** [fold_all f s ini] folds function [f] over all the samples contained in the
   saturated sample set [s], starting with accumulator [ini]. The order in which
   samples are processed is arbitrary. *)
val fold_all : (Sample.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [iter_all] is the expected special case of [fold_all] tailored to effectful
   operations. *)
val iter_all : (Sample.t -> unit) -> t -> unit

(** [fold_evals f s ini] folds function [f] over all the evaluation samples in
   the saturated sample set [s], starting with accumulator [ini]. The arguments
   to [f] give the basic term being evaluated and all the (sub)samples at which
   this evaluation occurs. The order in which evaluations are processed is
   arbitrary, but each basic term is processed exactly once. *)
val fold_evals : (Basic.t -> Sample.Set.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [iter_evals] is the expected special case of [fold_eval] tailored to
   effectful operations. *)
val iter_evals : (Basic.t -> Sample.Set.t -> unit) -> t -> unit

(** [fold_all f s ini] folds function [f] over all the basic terms [t] such that
    [Sample.last t] belongs to the saturated sample set [s], starting with
    accumulator [ini]. The order in which terms are processed is arbitrary. *)
val fold_lasts : (Basic.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [iter_lasts] is the expected special case of [fold_lasts] tailored to
    effectful operations. *)
val iter_lasts : (Basic.t -> unit) -> t -> unit
