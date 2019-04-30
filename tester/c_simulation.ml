open Base
open Lib
include C_simulation_intf

module Make (R : Sim_runner.S) : S with type t = R.t = struct
  type t = R.t

  let no_post_error () : Error.t =
    Error.of_string "This Litmus test doesn't have a postcondition."

  let post_from_test (_ : t) ~(input_path : Fpath.t)
      ~(output_path : Fpath.t) : post Or_error.t =
    ignore (output_path : Fpath.t) ;
    (* We don't need to talk to the simulator *)
    Or_error.Let_syntax.(
      let%bind ast = C.Frontend.Litmus.load ~path:input_path in
      let%bind mini = C.Mini_convert.litmus_of_raw_ast ast in
      let post = C.Mini_litmus.Ast.Validated.postcondition mini in
      Result.of_option post ~error:(no_post_error ()))

  let ext_from_test (ctx : t) ~(input_path : Fpath.t)
      ~(output_path : Fpath.t) : ext Or_error.t =
    Or_error.Let_syntax.(
      let%map output =
        R.run_and_load_results ctx ~input_path ~output_path
      in
      let state_list = Sim_output.states output in
      Sim_output.State.Set.of_list state_list)

  let run_source ~(input : 'a source)
      ~(from_test :
         input_path:Fpath.t -> output_path:Fpath.t -> 'a Or_error.t) :
      'a Or_error.t =
    match input with
    | Inline x ->
        Or_error.return x
    | From_test {input_path; output_path} ->
        from_test ~input_path ~output_path

  let run_post (ctx : t) : input:post source -> post Or_error.t =
    run_source ~from_test:(post_from_test ctx)

  let run_ext (ctx : t) : input:ext source -> ext Or_error.t =
    run_source ~from_test:(ext_from_test ctx)

  let run (ctx : t) ~(input : [`Post of post source | `Ext of ext source]) :
      [`Post of post | `Ext of ext] Or_error.t =
    match input with
    | `Post input ->
        Or_error.Let_syntax.(
          let%map o = run_post ctx ~input in
          `Post o)
    | `Ext input ->
        Or_error.Let_syntax.(
          let%map o = run_ext ctx ~input in
          `Ext o)
end
