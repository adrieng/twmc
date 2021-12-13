(** {1 Utility functions related to equality} *)

(** The type of equality testers. *)
type 'a t = 'a -> 'a -> bool

(** An equality tester for association lists. *)
val assoc_list : 'a t -> 'b t -> ('a * 'b) list t
