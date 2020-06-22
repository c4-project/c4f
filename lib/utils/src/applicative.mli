(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Applicative polyfills for Travesty.

    These exist because a lot of the more recent ACT traversal code hedges on
    a possible migration of Travesty to applicatives. If that migration does
    happen upstream, everything in this module will move upstream, and this
    module will disappear. *)

open Base

(** The identity applicative functor.

    This just directly wraps a type ['a], with maps of functions [f] just
    applying [f] directly. *)
module Ident : Applicative.S with type 'a t = 'a

(** As {!Base.Applicative.Of_monad}, but doesn't destructively substitute
    [t]. *)
module Of_monad_ext (M : Monad.S) : Applicative.S with type 'a t = 'a M.t
