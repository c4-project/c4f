(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* TODO(@MattWindsor91): migrate this to Travesty. *)

module Ident : Applicative.S with type 'a t = 'a = struct
  type 'a t = 'a

  include Applicative.Make (struct
    type 'a t = 'a

    let apply f a = f a

    let return x = x

    let map = `Custom (fun a ~f -> f a)
  end)
end

module Of_monad_ext (M : Monad.S) : Applicative.S with type 'a t = 'a M.t =
struct
  type 'a t = 'a M.t

  include Applicative.Of_monad (M)
end
