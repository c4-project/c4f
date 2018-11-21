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

open Core_kernel

(** [Cell] contains the type of the individual cells that make up a
    zipper. *)
module Cell = struct
  (** ['a t] is the type of one cell.  Each cell contains the
      data at the given zipper location, as well as any marks that
      have been attached to the cell for later recall. *)
  type 'a t =
    { data  : 'a
    ; marks : Int.Set.t
    } [@@deriving fields, sexp]
  ;;

  let make data = { data; marks = Int.Set.empty }
  let of_data_list = List.map ~f:make
  let to_data_list = List.map ~f:data

  let mark cell new_mark =
    { cell with marks = Int.Set.add cell.marks new_mark }
  ;;

  include Fold_map.Make_container1 (struct
      type nonrec 'a t = 'a t

      module On_monad (M : Monad.S) = struct
        let fold_mapM ~f ~init cell =
          M.(
            f init cell.data >>|
            Tuple2.map_snd ~f:(fun d -> { cell with data = d })
          )
        ;;
      end
    end)
  ;;
end

type 'a t =
  { left  : 'a Cell.t list
  ; right : 'a Cell.t list
  ; mark_counter : int
  } [@@deriving fields, sexp]
;;

let make ~left ~right =
  { left         = Cell.of_data_list left
  ; right        = Cell.of_data_list right
  ; mark_counter = 0
  }
;;

let of_list lst = make ~left:[] ~right:lst

let left_list zipper  = Cell.to_data_list (left zipper)
let right_list zipper = Cell.to_data_list (right zipper)

let to_list zipper =
  List.rev_append (left_list zipper) (right_list zipper)
;;

let%expect_test "to_list reverses a fully-leftwards zipper" =
  let zipper = make ~left:[19; 27; 64; 101; -5; 2] ~right:[] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_list zipper : int list)];
  [%expect {| (2 -5 101 64 27 19) |}]
;;

let%expect_test "to_list-of_list idempotent on non-empty list" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_list (of_list [19; 27; 64; 101; -5; 2]) : int list)];
  [%expect {| (19 27 64 101 -5 2) |}]
;;

let%expect_test "to_list-of_list idempotent on empty list" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_list (of_list []) : string list)];
  [%expect {| () |}]
;;

let head zipper = List.hd (right zipper)

let set_head_on_right right new_head =
  match right, new_head with
  | []     , None       -> []
  | []     , Some head' -> [head']
  | _::rest, None       -> rest
  | _::rest, Some head' -> head'::rest
;;

let set_head zipper new_head =
  { zipper with right = set_head_on_right zipper.right new_head }
;;

let push zipper ~value =
  { zipper with right = (Cell.make value)::zipper.right }
;;

let pop_opt zipper =
  match zipper.right with
  | []    -> None
  | x::xs -> Some (x.data, { zipper with right = xs })
;;

let fetch_and_increment_mark zipper =
  let mark = zipper.mark_counter in
  (mark, { zipper with mark_counter = mark + 1 })
;;

let left_length zipper = List.length zipper.left
let right_length zipper = List.length zipper.right

let rev_transfer amount ~src ~dst =
  if Int.(List.length src < amount)
  then None
  else
    let (to_transfer, src') = List.split_n src amount in
    let dst' = List.rev_append to_transfer dst in
    Some (src', dst')
;;

module On_monad (M : Monad.S) = struct
  module CM = Cell.On_monad (M)
  module CO = Cell.On_monad (Option)

  let fold_mapM_head zipper ~f ~init =
    let open M.Let_syntax in
    match head zipper with
    | None   -> return (init, zipper)
    | Some h ->
      let%map (init', h') = CM.fold_mapM ~f ~init h in
      (init', set_head zipper (CO.sequenceM h'))
  ;;

  let popM zipper ~on_empty =
    match pop_opt zipper with
    | None        -> on_empty zipper
    | Some (h, z) -> M.return (h, z)
  ;;

  let stepM ?(steps=1) zipper ~on_empty =
    let amount = Int.abs steps in
    match Ordering.of_int (Int.compare steps 0) with
    | Less ->
      (match rev_transfer amount ~src:zipper.left ~dst:zipper.right with
       | Some (l, r) -> M.return { zipper with left = l; right = r }
       | None -> on_empty zipper)
    | Equal -> M.return zipper
    | Greater ->
      (match rev_transfer amount ~src:zipper.right ~dst:zipper.left with
       | Some (r, l) -> M.return { zipper with left = l; right = r }
       | None -> on_empty zipper)
  ;;

  let rec foldM_until zipper ~f ~init ~finish =
    let open M.Let_syntax in
    match pop_opt zipper with
    | None -> finish init zipper
    | Some (hd, zipper') ->
      match%bind f init hd zipper' with
      | `Stop final -> M.return final
      | `Continue accum ->
        foldM_until zipper' ~f ~init:accum ~finish
      | `Replace_and_continue (hd', accum) ->
        push zipper' ~value:hd'
        |>  stepM ~on_empty:M.return
        >>= foldM_until ~f ~init:accum ~finish
  ;;

  let mapM_head zipper ~f ~on_empty =
    match head zipper with
    | None   -> on_empty zipper
    | Some h -> M.(CM.mapM ~f h >>| CO.sequenceM >>| set_head zipper)
  ;;

  let markM zipper ~on_empty =
    match head zipper with
    | None   -> on_empty zipper
    | Some h ->
      let (mark, zipper') = fetch_and_increment_mark zipper in
      let h' = Cell.mark h mark in
      M.return (mark, set_head zipper' (Some h'))
  ;;

  let recallM zipper ~mark ~on_empty =
    let rec mu zipper' =
      match head zipper' with
      | Some h when Int.Set.mem (Cell.marks h) mark ->
        M.return zipper'
      | Some _ | None ->
        M.(stepM ~steps:(-1) zipper ~on_empty >>= mu)
    in mu zipper
  ;;
end

module On_ident = On_monad (Monad.Ident)
module On_error = On_monad (Or_error)

let map_head = On_ident.mapM_head ~on_empty:Fn.id

let pop zipper = On_error.popM zipper
    ~on_empty:(fun _ ->
        Or_error.error_string "Tried to pop an exhausted zipper")
;;

let to_two_lists zipper = (left_list zipper, right_list zipper)

let%expect_test "zipper: pop non-empty" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let result = Or_error.(zipper |> pop >>| Tuple2.map_snd ~f:to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int * (int list * int list)) Or_error.t)];
  [%expect {| (Ok (101 ((19 27 64) (-5 2)))) |}]
;;

let%expect_test "zipper: pop empty" =
  let zipper = make ~left:[19; 27; 64; 101; -5; 2] ~right:[] in
  let result = Or_error.(zipper |> pop >>| Tuple2.map_snd ~f:to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int * (int list * int list)) Or_error.t)];
  [%expect {| (Error "Tried to pop an exhausted zipper") |}]
;;

let step ?steps zipper = On_error.stepM ?steps zipper
    ~on_empty:(fun zipper ->
        Or_error.error_s
          [%message "Zipper stepping went out of bounds"
            ~steps:(Option.value ~default:1 steps : int)
            ~left_bound:(left_length zipper : int)
            ~right_bound:(right_length zipper : int)
          ]
      )
;;

let%expect_test "zipper: step default, in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zlists = Or_error.(zipper |> step >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((101 19 27 64) (-5 2))) |}]
;;

let%expect_test "zipper: step default, out-of-bounds" =
  let zipper = make ~left:[19; 27; 64; 101; -5; 2] ~right:[] in
  let zlists = Or_error.(zipper |> step >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {|
    (Error
     ("Zipper stepping went out of bounds" (steps 1) (left_bound 6)
      (right_bound 0))) |}]
;;

let%expect_test "zipper: step forwards multiple, just-in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zlists = Or_error.(zipper |> step ~steps:3 >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((2 -5 101 19 27 64) ())) |}]
;;

let%expect_test "zipper: step forwards multiple, just-out-of-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zlists = Or_error.(zipper |> step ~steps:4 >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {|
    (Error
     ("Zipper stepping went out of bounds" (steps 4) (left_bound 3)
      (right_bound 3))) |}]
;;

let%expect_test "zipper: step backwards, in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zlists = Or_error.(zipper |> step ~steps:(-1) >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((27 64) (19 101 -5 2))) |}]
;;

let%expect_test "zipper: step backwards, out-of-bounds" =
  let zipper = make ~right:[19; 27; 64; 101; -5; 2] ~left:[] in
  let zlists = Or_error.(zipper |> step ~steps:(-1) >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {|
    (Error
     ("Zipper stepping went out of bounds" (steps -1) (left_bound 0)
      (right_bound 6))) |}]
;;

let%expect_test "zipper: step backwards multiple, just-in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zlists = Or_error.(zipper |> step ~steps:(-3) >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {| (Ok (() (64 27 19 101 -5 2))) |}]
;;

let%expect_test "zipper: step backwards multiple, just-out-of-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zlists = Or_error.(zipper |> step ~steps:(-4) >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (zlists : (int list * int list) Or_error.t)];
  [%expect {|
    (Error
     ("Zipper stepping went out of bounds" (steps -4) (left_bound 3)
      (right_bound 3))) |}]
;;

let fold_until = On_ident.foldM_until

let%expect_test "zipper: fold_until: partition on sign" =
  let zipper = of_list [0; 2; -11; 64; 92; -92; 4; -6; -10] in
  let lists =
    fold_until zipper
      ~init:[]
      ~finish:(fun acc zipper ->
          Or_error.return (List.rev acc, to_list zipper))
      ~f:(fun negatives k _zipper ->
          if Int.is_negative k
          then `Continue (k::negatives)
          else `Replace_and_continue (k, negatives))
  in
  Sexp.output_hum Out_channel.stdout
    [%sexp (lists : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((-11 -92 -6 -10) (0 2 64 92 4))) |}]
;;

let mark zipper = On_error.markM zipper
    ~on_empty:(fun _ ->
        Or_error.error_string "Tried to mark an exhausted zipper")
;;

let recall zipper ~mark = On_error.recallM zipper ~mark
    ~on_empty:(fun _ ->
        Or_error.error_s
          [%message "Couldn't find requested mark" ~mark:(mark : int)]
      )
;;

let%expect_test "mark/recall: valid example" =
  let open Or_error.Let_syntax in
  let zipper = of_list [19; 27; 64; 101; -5; 2] in
  let result = (
    let%bind (m, zipper) =
      zipper
      |> step ~steps:2 (* looking at 64 *)
      >>= mark
    in
    let%bind (_, zipper) =
      zipper
      |> step (* looking at 101 *)
      >>= pop (* now looking at -5 *)
    in
    zipper
    |> push ~value:64 (* now looking at (another) 64 *)
    |> recall ~mark:m (* should have jumped to first 64 *)
    >>| to_two_lists
  )
  in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((27 19) (64 64 -5 2))) |}]
;;
