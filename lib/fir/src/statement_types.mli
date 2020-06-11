(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: module signatures for statements *)

open Base

(** Module type defining various traversals over statements that depend on
    knowing the metadata of a statement. *)
module type S_with_meta = sig
  (* Type of resolved statement metadata. *)
  type t

  (** Traversing over atomic-action addresses. *)
  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

  (** Traversing over expressions. *)
  module On_expressions :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Expression.t

  (** Traversing over lvalues. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

  (** Traversing over primitive statements. *)
  module On_primitives :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Prim_statement.t
end

(** Common functionality of statement components. *)
module type S_common = sig
  type 'meta t

  (** We can traverse over the metadata. *)
  module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t

  val erase_meta : 'meta t -> unit t
  (** [erase_meta x] deletes all of [x]'s metadata. *)

  (** By fixing the metadata type, we can perform various forms of standard
      traversal. *)
  module With_meta (Meta : T) : S_with_meta with type t := Meta.t t
end

(** {1 Parametrised statement signatures}

    These two signatures exist because their implementing modules are
    mutually recursive.

    The implementations of these generally fix the type parameters at the top
    of the signature. *)

(** {2 Statements}

    Parametrised signature of statement implementations. *)
module type S_statement = sig
  type 'meta t [@@deriving sexp, compare, equal]

  (** Generally fixed to {!Statement.If.t}. *)
  type 'meta if_stm

  (** Generally fixed to {!Statement.While.t}. *)
  type 'meta while_loop

  (** {3 Constructors} *)

  val if_stm : 'meta if_stm -> 'meta t
  (** [if_stm ifs] lifts an if statement [ifs] to a statement. *)

  val while_loop : 'meta while_loop -> 'meta t
  (** [while_loop loop] lifts a while or do-while loop [loop] to a statement. *)

  val prim : 'meta -> Prim_statement.t -> 'meta t
  (** [prim meta p] lifts a primitive statement [p] to a statement, with
      metadata [meta]. *)

  (** {3 Accessors} *)

  val reduce_step :
       'meta t
    -> prim:('meta * Prim_statement.t -> 'result)
    -> if_stm:('meta if_stm -> 'result)
    -> while_loop:('meta while_loop -> 'result)
    -> 'result
  (** [reduce_step stm ~prim ~if_stm ~while_loop] applies the appropriate
      function of those given to [stm]. It does _not_ recursively reduce
      statements inside blocks. *)

  val is_prim_and : 'meta t -> f:(Prim_statement.t -> bool) -> bool
  (** [is_prim_and x ~f] lifts the predicate [f] over primitive statements to
      one over statements, being [false] over any non-primitive statement
      (including those that contain primitive statements for which [f] is
      true). *)

  val is_if_statement : 'meta t -> bool
  (** [is_if_statement stm] is true provided that [stm] is an if statement. *)

  val has_if_statements : 'meta t -> bool
  (** [has_if_statements stm] is true provided that [stm] is an if statement,
      or a composite statement for which [has_if_statements] is true for at
      least one sub-statement. *)

  val has_while_loops : 'meta t -> bool
  (** [has_while_loops stm] is true provided that [stm] is a while (or
      do-while) loop, or a composite statement for which [has_while_loops] is
      true for at least one sub-statement. *)

  val has_blocks_with_metadata : 'meta t -> predicate:('meta -> bool) -> bool
  (** [has_blocks_with_metadata stm ~predicate] is true provided that [stm]
      has at least one block for which [predicate] is true on that block's
      metadata.

      This is useful for tracking things like the existence of dead-code. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         'm1 t
      -> prim:('m1 * Prim_statement.t -> ('m2 * Prim_statement.t) M.t)
      -> if_stm:('m1 if_stm -> 'm2 if_stm M.t)
      -> while_loop:('m1 while_loop -> 'm2 while_loop M.t)
      -> 'm2 t M.t
  end

  include S_common with type 'meta t := 'meta t
end

(** {2 If statements}

    Parametrised signature of if-statement implementations. *)
module type S_if_statement = sig
  (** Generally fixed to {!Expression.t}. *)
  type 'meta expr

  (** Generally fixed to {!Statement.t}. *)
  type 'meta stm

  (** Type of if statements. *)
  type 'meta t = ('meta, 'meta stm) If.t [@@deriving sexp, compare, equal]

  (** {3 Constructors} *)

  val make :
       cond:'meta expr
    -> t_branch:('meta, 'meta stm) Block.t
    -> f_branch:('meta, 'meta stm) Block.t
    -> 'meta t
  (** [make ~cond t_branch f_branch] creates an if statement with condition
      [cond], true branch [t_branch], and false branch [f_branch]. *)

  (** {3 Accessors} *)

  val cond : 'meta t -> 'meta expr
  (** [cond ifs] gets [ifs]'s condition. *)

  val t_branch : 'meta t -> ('meta, 'meta stm) Block.t
  (** [t_branch ifs] gets [ifs]'s true branch. *)

  val f_branch : 'meta t -> ('meta, 'meta stm) Block.t
  (** [f_branch ifs] gets [ifs]'s false branch. *)

  (** {3 Traversing} *)

  include S_common with type 'meta t := 'meta t
end

(** {2 While loops}

    Parametrised signature of while-loop implementations. *)
module type S_while_loop = sig
  (** Generally fixed to {!Expression.t}. *)
  type 'meta expr

  (** Generally fixed to {!Statement.t}. *)
  type 'meta stm

  (** Type of while loops. *)
  type 'meta t = ('meta, 'meta stm) While.t [@@deriving sexp, compare, equal]

  (** {3 Constructors} *)

  val make :
       cond:'meta expr
    -> body:('meta, 'meta stm) Block.t
    -> kind:While.Kind.t
    -> 'meta t
  (** [make ~cond ~body ~kind] creates a while loop (with [~kind=While]) or a
      do-while loop (with [~kind=Do_while]) with condition [cond] and body
      [body]. *)

  (** {3 Accessors} *)

  val cond : 'meta t -> 'meta expr
  (** [cond loop] gets [loop]'s condition. *)

  val body : 'meta t -> ('meta, 'meta stm) Block.t
  (** [body loop] gets [loop]'s body. *)

  val kind : 'meta t -> While.Kind.t
  (** [kind loop] gets whether [loop] is a while loop or a do-while loop. *)

  (** {3 Traversing} *)

  include S_common with type 'meta t := 'meta t
end
