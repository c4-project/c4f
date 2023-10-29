(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor_base
end

module In = struct
  module Dir = struct
    type t = Left | Right [@@deriving sexp, equal, accessors]
  end

  type t = Const of Dir.t * Constant.t | Refl [@@deriving sexp, accessors]

  let const_at (d : Dir.t) : ('a, Constant.t, t, [< A.variant]) A.t =
    [%accessor
      A.(
        const
        @> variant
             ~match_:(fun (d2, k) ->
               if Dir.equal d d2 then First k else Second (d2, k) )
             ~construct:(fun k -> (d, k)) )]

  let zero' (d : Dir.t) : ('a, unit, t, [< A.variant]) A.t =
    [%accessor A.(const_at d @> Constant.Acc.zero)]

  let zero (d : Dir.t) : t = Const (d, Constant.int 0)

  let minus_one (d : Dir.t) : t = Const (d, Constant.int (-1))

  let true_ (d : Dir.t) : t = Const (d, Constant.truth)

  let false_ (d : Dir.t) : t = Const (d, Constant.falsehood)
end

module Out = struct
  type t = Const of Constant.t | Idem [@@deriving sexp, equal, accessors]

  let zero : t = Const (Constant.int 0)

  let true_ : t = Const Constant.truth

  let false_ : t = Const Constant.falsehood
end

type t = {in_: In.t; out_: Out.t} [@@deriving sexp, accessors]

let ( @-> ) (in_ : In.t) (out_ : Out.t) : t = {in_; out_}

let single_in_matching (type i) (out_ : Out.t) :
    (i, In.t, t, [< A.optional_getter]) A.t =
  A.optional_getter (fun r ->
      Base.Option.(
        some_if (Out.equal r.out_ out_) ()
        >>= fun (_ : 'a) -> A.get_option in_ r ) )

let in_matching (out_ : Out.t) : ('i, In.t, t list, [< A.many_getter]) A.t =
  A.(List.each @> single_in_matching out_)

let in_out_matching
    (in_acc : (unit, 'a, In.t, ([< A.many_getter] as 'b)) A.t) (out_ : Out.t)
    : (unit, 'a, t list, 'b) A.t =
  [%accessor A.(in_matching out_ @> in_acc)]

let has_in_out_matching (in_acc : (unit, 'a, In.t, 'd) A.t) (out_ : Out.t)
    (xs : t list) : bool =
  not (A.is_empty (in_out_matching in_acc out_) xs)
