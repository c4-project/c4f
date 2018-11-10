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

include Core
include Utils
include Language_symbol_intf

module Make (B : Basic)
  : S with type t = B.t = struct
  include B

  module Comp = struct
    include B
    include Comparator.Make (B)
  end

  module Set = struct
    module M = Set.Make_using_comparator (Comp)
    include M
    include My_set.Extend (M)

    let abstract = Abstract.Symbol.Set.map ~f:B.abstract
  end

  module R_map = struct
    module M = Map.Make_using_comparator (Comp)
    type sym = t [@@deriving compare, eq, sexp]

    type r_dest =
      | Identity
      | MapsTo of sym
    [@@deriving sexp, eq]
    ;;

    type t = r_dest M.t

    module Set = Set
    let make : Set.t -> t = Set.to_map ~f:(Fn.const Identity)

    let sources_of (rmap : t) d =
      let indirects =
        rmap
        |> M.filter ~f:(equal_r_dest (MapsTo d))
        |> M.to_sequence
        |> Sequence.map ~f:fst
        |> Sequence.to_list
      in
      let directs =
        match M.find rmap d with
        | Some Identity -> [d]
        | Some _ | None -> []
      in
      directs @ indirects
    ;;

    let dest_of = M.find

    let redirect ~src ~dst rmap =
      Or_error.(
        if equal_sym src dst
        then return (M.set rmap ~key:src ~data:Identity)
        else if Option.is_some (Map.find rmap dst)
        then
          error_s
            [%message "Tried to redirect to an existing source"
                ~src:(src : sym)
                ~dst:(dst : sym)
            ]
        else
          rmap
          |> M.set ~key:src ~data:(MapsTo dst)
          |> M.map ~f:(function
              | MapsTo dst' when equal_sym src dst' -> MapsTo dst
              | x -> x)
          |> return
      )
    ;;
  end

  let program_id_of sym =
    let asyms = B.abstract_demangle sym in
    let ids =
      List.filter_map asyms ~f:Abstract.Symbol.program_id_of
    in
    (* The demangled symbols should be in order of likelihood, so we
         take a gamble on the first one being the most likely program
         ID in case of a tie. *)
    List.hd ids
  ;;

  let is_program_label sym = Option.is_some (program_id_of sym)
end

module String_direct = Make (struct
    include String

    let abstract = Fn.id
    let abstract_demangle = List.return
    let of_string_opt = Option.some

    module OnStrings = struct
      type t = string
      type elt = string
      include Singleton.With_elt (String)
    end
  end)
;;

let%expect_test "String.R_map: dest_of example run" =
  let open Or_error.Let_syntax in
  String_direct.R_map.(
    let r =
      let%map map =
        (return (make (String_direct.Set.singleton "whiskey")))
        >>= redirect ~src:"alpha"   ~dst:"alpha"
        >>= redirect ~src:"bravo"   ~dst:"echo"
        >>= redirect ~src:"charlie" ~dst:"echo"
        >>= redirect ~src:"echo"    ~dst:"foxtrot"
      in
      let alpha   = String_direct.R_map.dest_of map "alpha" in
      let bravo   = String_direct.R_map.dest_of map "bravo" in
      let charlie = String_direct.R_map.dest_of map "charlie" in
      let delta   = String_direct.R_map.dest_of map "delta" in
      (alpha, bravo, charlie, delta)
    in
    Sexp.output_hum
      Out_channel.stdout
      [%sexp ( r : ( r_dest option
                     * r_dest option
                     * r_dest option
                     * r_dest option
                   ) Or_error.t
             )
      ]
  );
  [%expect {| (Ok ((Identity) ((MapsTo foxtrot)) ((MapsTo foxtrot)) ())) |}]

let%expect_test "String.R_map: dest_of example run" =
  let open Or_error.Let_syntax in
  String_direct.R_map.(
    let r =
      let%map map =
        (return (make (String_direct.Set.singleton "whiskey")))
        >>= redirect ~src:"alpha"   ~dst:"alpha"
        >>= redirect ~src:"bravo"   ~dst:"echo"
        >>= redirect ~src:"charlie" ~dst:"echo"
        >>= redirect ~src:"echo"    ~dst:"foxtrot"
      in
      let alpha   = String_direct.R_map.sources_of map "alpha" in
      let bravo   = String_direct.R_map.sources_of map "bravo" in
      let echo    = String_direct.R_map.sources_of map "echo" in
      let foxtrot = String_direct.R_map.sources_of map "foxtrot" in
      (alpha, bravo, echo, foxtrot)
    in
    Sexp.output_hum
      Out_channel.stdout
      [%sexp ( r : ( string list
                     * string list
                     * string list
                     * string list
                   ) Or_error.t
             )
      ]
  );
  [%expect {| (Ok ((alpha) () () (bravo charlie echo))) |}]
