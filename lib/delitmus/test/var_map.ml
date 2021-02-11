(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%test_module "with example map" =
  ( module struct
    module M = C4f_delitmus.Var_map
    module Li = C4f_common.Litmus_id
    module Ci = C4f_common.C_id
    module Ct = C4f_fir.Type
    module Rc = M.Record

    let map : M.t =
      C4f_common.Scoped_map.of_litmus_id_map
        (Map.of_alist_exn
           (module Li)
           [ ( Li.of_string "0:r0"
             , { M.Record.c_id= Ci.of_string "t0r0"
               ; c_type= Ct.int ()
               ; mapped_to= Global
               ; initial_value= Some (C4f_fir.Constant.Int 0) } )
           ; ( Li.of_string "1:r0"
             , { M.Record.c_id= Ci.of_string "t1r0"
               ; c_type= Ct.bool ()
               ; mapped_to= Global
               ; initial_value= Some C4f_fir.Constant.falsehood } )
           ; ( Li.of_string "1:tmp"
             , { M.Record.c_id= Ci.of_string "t1tmp"
               ; c_type= Ct.int ()
               ; mapped_to= Param 1
               ; initial_value= Some (C4f_fir.Constant.Int 0) } )
           ; ( Li.of_string "x"
             , { M.Record.c_id= Ci.of_string "x"
               ; c_type= Ct.int ~is_atomic:true ()
               ; mapped_to= Global
               ; initial_value= Some (C4f_fir.Constant.Int 0) } )
           ; ( Li.of_string "y"
             , { M.Record.c_id= Ci.of_string "y"
               ; c_type= Ct.int ~is_atomic:true ()
               ; mapped_to= Param 0
               ; initial_value= Some (C4f_fir.Constant.Int 0) } ) ] )

    let print_mappings : (Li.t, M.Record.t) List.Assoc.t -> unit =
      List.iter ~f:(fun (x, v) ->
          printf "%s -> %s\n" (Li.to_string x) (Ci.to_string v.M.Record.c_id) )

    let%expect_test "globally_mapped_vars" =
      print_mappings (M.globally_mapped_vars map) ;
      [%expect {|
      x -> x
      0:r0 -> t0r0
      1:r0 -> t1r0 |}]

    let%expect_test "param_mapped_vars" =
      print_mappings (M.param_mapped_vars map) ;
      [%expect {|
      y -> y
      1:tmp -> t1tmp |}]

    let%expect_test "params_for_thread 0" =
      print_mappings (M.params_for_thread map 0) ;
      [%expect {| y -> y |}]

    let%expect_test "params_for_thread 1" =
      print_mappings (M.params_for_thread map 1) ;
      [%expect {|
      y -> y
      1:tmp -> t1tmp |}]
  end )
