open Base
include C_sim_intf
module C = Act_c
module Sim = Act_sim

module Make (R : Sim.Runner.S) : S = struct
  let no_post_error () : Error.t =
    Error.of_string "This Litmus test doesn't have a postcondition."

  let post_from_test ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      post Or_error.t =
    ignore (output_path : Fpath.t) ;
    (* We don't need to talk to the simulator *)
    Or_error.Let_syntax.(
      let%bind ast = C.Frontend.Litmus.load ~path:input_path in
      let%bind mini = C.Mini_convert.litmus_of_raw_ast ast in
      let post = C.Mini_litmus.Ast.Validated.postcondition mini in
      Result.of_option post ~error:(no_post_error ()))

  let ext_from_test ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      ext Or_error.t =
    Or_error.Let_syntax.(
      let%bind output = R.run Sim.Arch.C ~input_path ~output_path in
      let%map obs =
        Sim.Output.to_observation_or_error output ~handle_skipped:`Error
      in
      let state_list = Sim.Output.Observation.states obs in
      Set.of_list (module Sim.State) state_list)

  let run_source (input : 'a source)
      ~(from_test :
         input_path:Fpath.t -> output_path:Fpath.t -> 'a Or_error.t) :
      'a Or_error.t =
    match input with
    | Inline x ->
        Or_error.return x
    | From_test {input_path; output_path} ->
        from_test ~input_path ~output_path

  let run_post : post source -> post Or_error.t =
    run_source ~from_test:post_from_test

  let run_ext : ext source -> ext Or_error.t =
    run_source ~from_test:ext_from_test

  let run :
         [`Post of post source | `Ext of ext source]
      -> [`Post of post | `Ext of ext] Or_error.t = function
    | `Post input ->
        Or_error.Let_syntax.(
          let%map o = run_post input in
          `Post o)
    | `Ext input ->
        Or_error.Let_syntax.(
          let%map o = run_ext input in
          `Ext o)
end
