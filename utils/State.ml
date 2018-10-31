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

(* We implement [Make] in terms of [Make_transform] and the identity
   monad, for simplicity. *)

module Make_transform (B : Basic_transform)
  : S_transform with type state = B.t and module Inner = B.Inner = struct
  type state = B.t
  module Inner = B.Inner

  module T = struct
    type 'a t = (state -> (state * 'a) Inner.t)

    include Monad.Make (struct
        type nonrec 'a t = 'a t

        let map' wc ~f =
          fun state ->
            let open Inner.Let_syntax in
            let%map (state', a) = wc state in
            (state', f a)
        ;;

        let map = `Custom map'

        let bind wc ~f =
          fun state ->
            let open Inner.Let_syntax in
            let%bind (state', a) = wc state in
            (f a) state'
        ;;

        let return a = fun state -> Inner.return (state, a);;
      end)
  end

  include T
  include MyMonad.Extend(T)

  let run f ctx = f ctx |> Inner.map ~f:snd

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

    let return (x : 'a Inner.t) ctx =
      Inner.(x >>| Tuple2.create ctx)
  end

  let make f = Monadic.make (Fn.compose Inner.return f)
  let peek f = Monadic.peek (Fn.compose Inner.return f)
  let modify f = Monadic.modify (Fn.compose Inner.return f)
end

module Make (B : Basic)
  : S with type state = B.t = struct
  include Make_transform (struct
      type t = B.t
      module Inner = Monad.Ident
    end)

  let on_fold_map
      (mapper : (state, 'a, 'b) fold_mapper)
      (comp   : ('a -> 'a t))
      (coll   : 'b) =
    make
      (fun ctx ->
         mapper coll
         ~init:ctx
         ~f:(fun ctx' b ->
             run
               (let open Let_syntax in
                let%bind b' = comp b in
                let%map ctx'' = peek Fn.id in
                (ctx'', b')
               )
               ctx')
      )
end
;;
