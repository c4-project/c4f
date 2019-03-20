(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

open Base
open Utils
include Redirect_map_intf

module Make (B : Basic_symbol) : S with type sym := B.t = struct
  type t = (B.t, B.t) List.Assoc.t [@@deriving sexp]

  let resolve_sym (map : t)
                  (symbol : B.t) : B.t Or_error.t =
    symbol
    |> List.Assoc.find map ~equal:[%equal: B.t]
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "Couldn't resolve symbol in redirects table"
                  ~here:[%here]
                  ~symbol:(symbol : B.t)
                  ~redirects:(map : t)])
  ;;

  let resolve_id (map : t)
                 (id : C_identifier.t) : C_identifier.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind sym = B.of_c_identifier id in
    let%bind redirected_sym = resolve_sym map sym in
    B.to_c_identifier redirected_sym
  ;;

  let image_ids (map : t) : C_identifier.Set.t Or_error.t =
    let open Or_error.Monad_infix in
    map
    |> List.map ~f:(fun (_, sym) -> B.to_c_identifier sym)
    |> Or_error.combine_errors
    >>| C_identifier.Set.of_list
  ;;

  let of_symbol_alist : (B.t, B.t) List.Assoc.t -> t = Fn.id

  let to_string_alist (map : t) : (string, string) List.Assoc.t =
    Travesty.T_alist.bi_map map ~left:B.to_string ~right:B.to_string
  ;;

  (** [transform_c_variables map cvars] tries to apply the redirects in
      [map] to [cvars]. *)
  let transform_c_variables
    (map : t)
    (cvars : Config.C_variables.Map.t)
    : Config.C_variables.Map.t Or_error.t =
    let open Or_error.Monad_infix in
    cvars
    |> Map.to_alist
    |> List.map ~f:(fun (var, record) ->
        var |> resolve_id map >>| fun v' -> (v', record)
      )
    |> Or_error.combine_errors
    >>= C_identifier.Map.of_alist_or_error
end

module Make_from_language_symbol (LS : Language_symbol.S) : S with type sym := LS.t =
Make (struct
  include LS

  let of_string (x : string) = Option.value_exn (LS.of_string_opt x)

  let to_c_identifier (s : LS.t) : C_identifier.t Or_error.t =
    s |> LS.to_string |> C_identifier.create
  ;;

  let of_c_identifier (id : C_identifier.t) : LS.t Or_error.t =
    id
    |> C_identifier.to_string
    |> LS.of_string_opt
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "Couldn't convert identifier to symbol"
                  ~here:[%here]
                  ~id:(id : C_identifier.t)])
  ;;
end)
