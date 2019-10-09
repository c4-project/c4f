(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type 'aux t = {aux: 'aux; input: Input.t; output: Output.t}
[@@deriving make, fields]

let input_path_string (ctx : _ t) : string = Input.to_string (input ctx)

module On_aux : Travesty.Traversable_types.S1 with type 'a t := 'a t =
Travesty.Traversable.Make1 (struct
  type nonrec 'a t = 'a t

  module On_monad (M : Monad.S) = struct
    let map_m (ctx : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
      M.Let_syntax.(
        let aux = aux ctx in
        let%map aux' = f aux in
        {ctx with aux= aux'})
  end
end)
