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

  let mark cell ~mark =
    { cell with marks = Int.Set.add cell.marks mark }
  ;;

  include Traversable.Make_container1 (struct
      type nonrec 'a t = 'a t

      module On_monad (M : Monad.S) = struct
        let map_m cell ~f =
          M.(f cell.data >>| fun d -> { cell with data = d })
        ;;
      end
    end)
  ;;
end

type 'a t =
  { left  : 'a Cell.t list
  ; right : 'a Cell.t list
  } [@@deriving fields, sexp]
;;

let make ~left ~right =
  { left  = Cell.of_data_list left
  ; right = Cell.of_data_list right
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

let set_head_cell_on_right right new_head =
  match right, new_head with
  | []     , None       -> []
  | []     , Some head' -> [head']
  | _::rest, None       -> rest
  | _::rest, Some head' -> head'::rest
;;

let set_head_cell zipper new_head =
  { zipper with right = set_head_cell_on_right zipper.right new_head }
;;

let push zipper ~value =
  { zipper with right = (Cell.make value)::zipper.right }
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

(* We split On_monad into two bits so we can use the option-monad
   specialisation of some of the monadic operations to define some
   of the others. *)
module On_monad_base (M : Monad.S) = struct
  module CM = Cell.On_monad (M)
  module CO = Cell.On_monad (Option)

  let pop_m zipper ~on_empty =
    match zipper.right with
    | []    -> on_empty zipper
    | x::xs -> M.return (x.data, { zipper with right = xs })
end

module On_option_base = On_monad_base (Option)
let pop_opt zipper =
  On_option_base.pop_m ~on_empty:(Fn.const None) zipper
;;

let peek_opt ?(steps=0) zipper =
  let open Option.Let_syntax in
  let%map cell =
    if steps < 0
    then List.nth zipper.left  ((Int.abs steps) - 1)
    else List.nth zipper.right steps
  in
  cell.data
;;

let%expect_test "peek_opt: default, in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt zipper : int option)];
  [%expect {| (101) |}]
;;

let%expect_test "peek_opt: default, out-of-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt zipper : int option)];
  [%expect {| () |}]
;;

let%expect_test "peek_opt: directly backwards, in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt ~steps:(-1) zipper : int option)];
  [%expect {| (19) |}]
;;

let%expect_test "peek_opt: directly backwards, out-of-bounds" =
  let zipper = make ~left:[] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt ~steps:(-1) zipper : int option)];
  [%expect {| () |}]
;;

let%expect_test "peek_opt: several steps forwards, in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt ~steps:2 zipper : int option)];
  [%expect {| (2) |}]
;;

let%expect_test "peek_opt: several steps forwards, out-of-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt ~steps:3 zipper : int option)];
  [%expect {| () |}]
;;

let%expect_test "peek_opt: several steps backwards, in-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt ~steps:(-3) zipper : int option)];
  [%expect {| (64) |}]
;;

let%expect_test "peek_opt: several steps forwards, out-of-bounds" =
  let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  Sexp.output_hum Out_channel.stdout
    [%sexp (peek_opt ~steps:(-4) zipper : int option)];
  [%expect {| () |}]
;;

(** [fold_outcome] is the type of instructions returned by functions
   used with [fold_untilM] and [fold_until]. *)
type ('a, 'acc, 'final) fold_outcome =
  [ `Stop of 'final          (** Stop folding and immediately return *)
  | `Drop of            'acc (** Drop the cursor and continue *)
  | `Swap of       'a * 'acc (** Replace cursor with a new value *)
  | `Mark of int * 'a * 'acc (** Replace, and mark, the cursor *)
  ]
;;

module On_monad (M : Monad.S) = struct
  include On_monad_base (M)

  let peek_m ?steps zipper ~on_empty =
    match peek_opt ?steps zipper with
    | Some v -> M.return v
    | None   -> on_empty zipper
  ;;

  let step_m ?(steps=1) zipper ~on_empty =
    let amount = Int.abs steps in
    match Ordering.of_int (Int.compare steps 0) with
    | Less ->
      (match rev_transfer amount ~src:zipper.left ~dst:zipper.right with
       | Some (l, r) -> M.return { left = l; right = r }
       | None -> on_empty zipper)
    | Equal -> M.return zipper
    | Greater ->
      (match rev_transfer amount ~src:zipper.right ~dst:zipper.left with
       | Some (r, l) -> M.return { left = l; right = r }
       | None -> on_empty zipper)
  ;;

  let map_m_head_cell zipper ~f ~on_empty =
    match head zipper with
    | None   -> on_empty zipper
    | Some h -> M.(f h >>| set_head_cell zipper)
  ;;

  let map_m_head zipper ~f ~on_empty =
    map_m_head_cell zipper
      ~f:M.(fun h -> CM.map_m ~f h >>| CO.sequence_m)
      ~on_empty
  ;;

  let mark_m zipper ~mark ~on_empty =
    map_m_head_cell zipper
      ~f:(fun h -> M.return (Some (Cell.mark ~mark h)))
      ~on_empty
  ;;

  let rec fold_m_until zipper ~f ~init ~finish =
    let open M.Let_syntax in
    match pop_opt zipper with
    | None -> finish init zipper
    | Some (hd, zipper') ->
      match%bind f init hd zipper' with
      | `Stop final -> M.return final
      | `Drop accum ->
        fold_m_until zipper' ~f ~init:accum ~finish
      | `Mark (mark, hd', accum) ->
        push zipper' ~value:hd'
        |>  mark_m ~mark ~on_empty:M.return
        >>= step_m ~on_empty:M.return
        >>= fold_m_until ~f ~init:accum ~finish
      | `Swap (hd', accum) ->
        push zipper' ~value:hd'
        |>  step_m ~on_empty:M.return
        >>= fold_m_until ~f ~init:accum ~finish
  ;;

  let recall_m zipper ~mark ~on_empty =
    let rec mu zipper' =
      match head zipper' with
      | Some h when Int.Set.mem (Cell.marks h) mark ->
        M.return zipper'
      | Some _ | None ->
        M.(step_m ~steps:(-1) zipper' ~on_empty >>= mu)
    in mu zipper
  ;;

  let delete_to_mark_m zipper ~mark ~on_empty =
    let open M.Let_syntax in
    let%map recalled_zipper = recall_m zipper ~mark ~on_empty in
    let amount_to_delete =
      left_length zipper - left_length recalled_zipper
    in
    { zipper with left = List.drop zipper.left amount_to_delete }
  ;;
end

module On_ident = On_monad (Monad.Ident)
module On_error = On_monad (Or_error)

let to_two_lists zipper = (left_list zipper, right_list zipper)

let map_head = On_ident.map_m_head ~on_empty:Fn.id

let%expect_test "map_head, present head, no removal" =
  let zipper  = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zipper' = map_head ~f:(fun x -> Some (x * 2)) zipper in
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_two_lists zipper' : (int list * int list))];
  [%expect {| ((19 27 64) (202 -5 2)) |}]
;;

let%expect_test "map_head, present head, removal" =
  let zipper  = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
  let zipper' = map_head ~f:(Fn.const None) zipper in
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_two_lists zipper' : (int list * int list))];
  [%expect {| ((19 27 64) (-5 2)) |}]
;;

let%expect_test "map_head, absent head, no removal" =
  let zipper  = make ~left:[19; 27; 64] ~right:[] in
  let zipper' = map_head ~f:(fun x -> Some (x * 2)) zipper in
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_two_lists zipper' : (int list * int list))];
  [%expect {| ((19 27 64) ()) |}]
;;

let%expect_test "map_head, absent head, removal" =
  let zipper  = make ~left:[19; 27; 64] ~right:[] in
  let zipper' = map_head ~f:(Fn.const None) zipper in
  Sexp.output_hum Out_channel.stdout
    [%sexp (to_two_lists zipper' : (int list * int list))];
  [%expect {| ((19 27 64) ()) |}]
;;

let pop zipper = On_error.pop_m zipper
    ~on_empty:(fun _ ->
        Or_error.error_string "Tried to pop an exhausted zipper")
;;

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

let step ?steps zipper = On_error.step_m ?steps zipper
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

let fold_until = On_ident.fold_m_until

let%expect_test "zipper: fold_until: partition on sign" =
  let zipper = of_list [0; 2; -11; 64; 92; -92; 4; -6; -10] in
  let lists =
    fold_until zipper
      ~init:[]
      ~finish:(fun acc zipper ->
          Or_error.return (List.rev acc, to_list zipper))
      ~f:(fun negatives k _zipper ->
          if Int.is_negative k
          then `Drop (k::negatives)
          else `Swap (k, negatives))
  in
  Sexp.output_hum Out_channel.stdout
    [%sexp (lists : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((-11 -92 -6 -10) (0 2 64 92 4))) |}]
;;

let mark zipper ~mark = On_error.mark_m zipper ~mark
    ~on_empty:(fun _ ->
        Or_error.error_string "Tried to mark an exhausted zipper")
;;

let mark_not_found mark =
  Or_error.error_s
    [%message "Couldn't find requested mark" ~mark:(mark : int)]
;;

let recall zipper ~mark = On_error.recall_m zipper ~mark
    ~on_empty:(fun _ -> mark_not_found mark)
;;

let mark_recall_example () =
  Or_error.(
    of_list [19; 27; 64; 101; -5; 2]
    |> step ~steps:2 (* looking at 64 *)
    >>= mark ~mark:1
    >>= step (* looking at 101 *)
    >>= pop  (* now looking at -5 *)
    >>| Tuple2.get2
  )
;;

let%expect_test "mark/recall example (without marking or recalling)" =
  let result = Or_error.(mark_recall_example () >>| to_two_lists) in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((64 27 19) (-5 2))) |}]
;;

let%expect_test "mark/recall: valid example" =
  let result = Or_error.(
    mark_recall_example ()
    >>| push ~value:64 (* now looking at (another) 64 *)
    >>= recall ~mark:1 (* should have jumped to first 64 *)
    >>| to_two_lists
  )
  in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((27 19) (64 64 -5 2))) |}]
;;


let delete_to_mark zipper ~mark =
  On_error.delete_to_mark_m zipper ~mark
    ~on_empty:(fun _ -> mark_not_found mark)
;;

let%expect_test "mark/delete_to_mark_incl: valid example" =
  let result = Or_error.(
    mark_recall_example ()
    >>| push ~value:27
    >>| push ~value:53
    >>= step ~steps:2
    >>= delete_to_mark ~mark:1
    >>| to_two_lists
  )
  in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int list * int list) Or_error.t)];
  [%expect {| (Ok ((27 19) (-5 2))) |}]
;;


let%expect_test "mark/delete_to_mark_incl: deleting to non-existent mark" =
  let result = Or_error.(
    mark_recall_example ()
    >>= delete_to_mark ~mark:2
    >>| to_two_lists
  )
  in
  Sexp.output_hum Out_channel.stdout
    [%sexp (result : (int list * int list) Or_error.t)];
  [%expect {| (Error ("Couldn't find requested mark" (mark 2))) |}]
;;
