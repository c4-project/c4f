(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core

include State_intf

(* We implement all of the various monads and functors in terms of
   [Make2_transform], for simplicity. *)
module Make2_transform (M : Monad.S)
  : S2_transform with module Inner = M = struct
  module Inner = M

  module T = struct
    type ('a, 's) t = ('s -> ('s * 'a) Inner.t)

    include Monad.Make2 (struct
        type nonrec ('a, 's) t = ('a, 's) t

        let map' wc ~f state =
          let open Inner.Let_syntax in
          let%map (state', a) = wc state in
          (state', f a)
        ;;

        let map = `Custom map'

        let bind wc ~f state =
          let open Inner.Let_syntax in
          let%bind (state', a) = wc state in
          (f a) state'
        ;;

        let return a state = Inner.return (state, a)
      end)
  end

  include T

  let run' f ctx = f ctx
  let run  f ctx = f ctx |> Inner.map ~f:snd

  module Monadic = struct
    let make = Fn.id

    let peek f ctx =
      let open Inner.Let_syntax in
      let%map v = f ctx in (ctx, v)
    ;;

    let modify f ctx =
      let open Inner.Let_syntax in
      let%map ctx' = f ctx in (ctx', ())
    ;;

    let return (x : 'a Inner.t) ctx = Inner.(x >>| Tuple2.create ctx)
  end

  let make f = Monadic.make (Fn.compose Inner.return f)
  let peek f = Monadic.peek (Fn.compose Inner.return f)
  let modify f = Monadic.modify (Fn.compose Inner.return f)

  let fix ~f init =
    let rec mu a ctx =
      let mu_monad x = Monadic.make (mu x) in
      let f' =
        Let_syntax.(
          let%bind a'   = f mu_monad a in
          let%map  ctx' = peek (Fn.id) in
          (ctx', a')
        )
      in
      run f' ctx
    in
    Monadic.make (mu init)
  ;;
end

module M2 : S2 = Make2_transform (Monad.Ident)

module To_S_transform (M : S2_transform) (B : Base.T)
  : S_transform with type state = B.t and module Inner = M.Inner = struct
  type state = B.t

  module M1 = struct
    type 'a t = ('a, state) M.t
    include My_monad.S2_to_S (M) (B)
  end
  include M1
  include My_monad.Extend (M1)

  include (M : Generic_transform with type ('a, 's) t := 'a t
                                  and type 's state := state
                                  and module Inner = M.Inner)

end

module Make_transform (B : Basic_transform)
  : S_transform with type state = B.t and module Inner = B.Inner =
  To_S_transform (Make2_transform (B.Inner)) (B)
;;

module To_S (M : S2) (B : Base.T)
  : S with type state = B.t = struct
  type state = B.t

  module M1 = struct
    type 'a t = ('a, state) M.t
    include My_monad.S2_to_S (M) (B)
  end
  include M1
  include My_monad.Extend (M1)

  include (M : Generic with type ('a, 's) t := 'a t
                        and type 'a final := 'a
                        and type 's state := state)
end

module Make (B : T)
  : S with type state = B.t =
  Make_transform (struct
    type t = B.t
    module Inner = Monad.Ident
  end)
;;
