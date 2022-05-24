(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* TODO(@MattWindsor91): migrate to Travesty *)

module On_monad (M : Monad.S) :
  Reader_types.S2 with type 'r Inner.t = 'r M.t = struct
  module A = struct
    type ('r, 'k) t = ('k -> 'r M.t) Staged.t
  end

  include A

  (* Thus named in case this gets ported to travesty *)
  module Inner = struct
    type 'r t = 'r M.t

    let lift = Staged.stage

    let return (x : 'r M.t) : ('r, 'k) A.t = lift (Fn.const x)
  end

  let lift (type k r) (f : k -> r) : (r, k) t =
    Inner.lift (fun x -> M.return (f x))

  let run (type k r) (f : (r, k) t) ~(ctx : k) : r M.t = Staged.unstage f ctx

  include (
    Monad.Make2 (struct
      type nonrec ('r, 'k) t = ('r, 'k) t

      let return (x : 'r) : ('r, 'k) t = Inner.return (M.return x)

      let bind (g : ('a, 'k) t) ~(f : 'a -> ('b, 'k) t) : ('b, 'k) t =
        Inner.lift (fun ctx ->
            M.Let_syntax.(
              let%bind x = run g ~ctx in
              run (f x) ~ctx) )

      let map' (g : ('a, 'k) t) ~(f : 'a -> 'b) : ('b, 'k) t =
        Inner.lift (fun ctx -> M.(run g ~ctx >>| f))

      let map = `Custom map'
    end) :
      Monad.S2 with type ('k, 'r) t := ('k, 'r) t )

  let ( let+ ) x f = map x ~f

  let ( let* ) x f = bind x ~f
end

module With_errors : Reader_types.S2 with type 'r Inner.t = 'r Or_error.t =
  On_monad (Or_error)

module Fix_context (R : Reader_types.S2) (B : T) :
  Reader_types.S
    with type 'r t = ('r, B.t) R.t
     and type 'r Inner.t = 'r R.Inner.t
     and type ctx = B.t = struct
  type 'r t = ('r, B.t) R.t

  type ctx = B.t

  include Travesty.Monad_exts.S2_to_S (R) (B)

  let lift = R.lift

  let run = R.run

  module Inner = struct
    type 'a t = 'a R.Inner.t

    let lift = R.Inner.lift

    let return = R.Inner.return
  end

  let ( let+ ) x f = map x ~f

  let ( let* ) x f = bind x ~f
end

include On_monad (Monad.Ident)
