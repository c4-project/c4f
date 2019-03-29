(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
open Utils

(* The unusual module nesting here is to make use of various extensions and
   avoid various shadowings. *)
module Single_passes = struct
  module M = struct
    type t =
      [ `Escape_symbols
      | `Language_hooks
      | `Remove_boundaries
      | `Remove_litmus
      | `Remove_useless
      | `Simplify_deref_chains
      | `Simplify_litmus
      | `Unmangle_symbols
      | `Warn ]
    [@@deriving enum, enumerate]

    let table =
      [ (`Escape_symbols, "escape-symbols")
      ; (`Language_hooks, "language-hooks")
      ; (`Remove_boundaries, "remove-boundaries")
      ; (`Remove_litmus, "remove-litmus")
      ; (`Remove_useless, "remove-useless")
      ; (`Simplify_deref_chains, "simplify-deref-chains")
      ; (`Simplify_litmus, "simplify-litmus")
      ; (`Unmangle_symbols, "unmangle-symbols")
      ; (`Warn, "warn") ]
  end

  include M
  module ET = Enum.Extend_table (M)
  include ET

  let tree_docs : Property.Tree_doc.t =
    [ ( "escape-symbols"
      , { args= []
        ; details= {| Mangle symbols to ensure litmus tools can lex them. |}
        } )
    ; ( "language-hooks"
      , { args= []
        ; details=
            {| Run language-specific hooks.
             Said hooks may be further categorised into passes, so
             enabling language-hooks on its own won't enable all
             language-specific passes. |}
        } )
    ; ( "remove-boundaries"
      , { args= []
        ; details=
            {| Remove program boundaries.
             If this pass isn't active, program boundaries are retained
             even if they're not jumped to. |}
        } )
    ; ( "remove-litmus"
      , { args= []
        ; details=
            {| Remove elements that have an effect in the assembly, but
             said effect isn't captured in the litmus test. |}
        } )
    ; ( "remove-useless"
      , { args= []
        ; details=
            {| Remove elements with no (direct) effect in the assembly. |}
        } )
    ; ( "simplify-deref-chains"
      , { args= []
        ; details=
            {| Replace 'deref chains' with direct movements.  This is a
             fairly heavyweight change. |}
        } )
    ; ( "simplify-litmus"
      , { args= []
        ; details=
            {| Perform relatively minor simplifications on elements that
             aren't directly understandable by litmus tools. |}
        } )
    ; ( "unmangle-symbols"
      , { args= []
        ; details=
            {| Where possible, replace symbols with their original C
            identifiers. |}
        } )
    ; ( "warn"
      , { args= []
        ; details= {| Warn about things the sanitiser doesn't understand. |}
        } ) ]
end

include Single_passes

let%expect_test "all passes accounted for" =
  Fmt.(pr "@[<v>%a@]@." (list ~sep:cut pp)) (all_list ()) ;
  [%expect
    {|
    escape-symbols
    language-hooks
    remove-boundaries
    remove-litmus
    remove-useless
    simplify-deref-chains
    simplify-litmus
    unmangle-symbols
    warn |}]

let all_lazy = lazy (all_set ())

let explain = Set.of_list [`Remove_useless]

let standard = all_set ()

module Selector = struct
  type elt = Single_passes.t [@@deriving eq, enumerate]

  module Category = struct
    module M = struct
      type t = [`Standard | `Explain] [@@deriving enum, enumerate]

      let table = [(`Standard, "%standard"); (`Explain, "%explain")]

      let tree_docs : Property.Tree_doc.t =
        [ ( "%standard"
          , { args= []
            ; details=
                {| Set containing all sanitiser passes that are considered
                 unlikely to change program semantics. |}
            } )
        ; ( "%explain"
          , { args= []
            ; details=
                {| Set containing only sanitiser passes that aid readability
                 when reading assembly, for example directive removal. |}
            } ) ]
    end

    include M
    include Enum.Extend_table (M)

    let __t_of_sexp__ = t_of_sexp (* ?! *)

    let%expect_test "all categories accounted for" =
      Fmt.(pr "@[<v>%a@]@." (list ~sep:cut pp) (all_list ())) ;
      [%expect {|
        %standard
        %explain |}]
  end

  module M = struct
    type t = [elt | Category.t | `Default] [@@deriving eq, enumerate]

    let table =
      List.concat
        [ List.map ~f:(fun (k, v) -> ((k :> t), v)) Single_passes.table
        ; List.map ~f:(fun (k, v) -> ((k :> t), v)) Category.table
        ; [(`Default, "%default")] ]
  end

  include M

  include Enum.Extend_table (struct
    include M
    include Enum.Make_from_enumerate (M)
  end)

  let __t_of_sexp__ = t_of_sexp (* ?! *)

  let eval (default : Single_passes.Set.t) : t -> Single_passes.Set.t =
    function
    | #elt as pass ->
        Single_passes.Set.singleton pass
    | `Standard ->
        standard
    | `Explain ->
        explain
    | `Default ->
        default

  let tree_docs : Property.Tree_doc.t =
    List.concat
      [ Single_passes.tree_docs
      ; Category.tree_docs
      ; [ ( "%default"
          , { args= []
            ; details=
                {| Set containing whichever sanitiser passes are the
                 default for the particular act subcommand. |}
            } ) ] ]

  let%expect_test "all passes have documentation" =
    let num_passes =
      all |> List.map ~f:to_string
      |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
      |> List.count ~f:not
    in
    Fmt.pr "@[<v>%d@]@." num_passes ;
    [%expect {| 0 |}]

  let pp_tree : unit Fmt.t =
    Property.Tree_doc.pp tree_docs (List.map ~f:snd table)

  let eval_b pred ~default =
    Blang.eval_set ~universe:all_lazy (eval default) pred

  let%expect_test "eval_b: standard and not explain" =
    let blang = Blang.O.(base `Standard && not (base `Explain)) in
    Sexp.output_hum Out_channel.stdout
      [%sexp
        (eval_b blang ~default:Single_passes.Set.empty : Single_passes.Set.t)] ;
    [%expect
      {|
      (escape-symbols language-hooks remove-boundaries remove-litmus
       simplify-deref-chains simplify-litmus unmangle-symbols warn) |}]
end
