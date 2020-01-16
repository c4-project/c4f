(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let%test_module "dry runs" =
  ( module struct
    module Scp = Plumbing.Scp.Using_runner (Plumbing.Runner.Dry_run)

    let ssh_with_user : Plumbing.Ssh.t =
      Plumbing.Ssh.make ~user:"piers" ~host:"spikemuth" ()

    let prerr : unit Or_error.t -> unit =
      Result.iter_error ~f:(Fmt.pr "@[ERROR:@ %a@]@." Error.pp)

    let%expect_test "single send with user" =
      prerr
        (Scp.send ssh_with_user ~recurse:false ~locals:[Fpath.v "foo"]
           ~remote:"docs/foo") ;
      [%expect
        {|
        RUN scp
        - arg: -q
        - arg: -B
        - arg: foo
        - arg: piers@spikemuth:docs/foo |}]

    let%expect_test "multiple send with user" =
      prerr
        (Scp.send ssh_with_user ~recurse:false
           ~locals:Fpath.[v "foo"; v "bar"; v "baz"]
           ~remote:"docs/") ;
      [%expect
        {|
        RUN scp
        - arg: -q
        - arg: -B
        - arg: foo
        - arg: bar
        - arg: baz
        - arg: piers@spikemuth:docs/ |}]

    let%expect_test "recursive send with user" =
      prerr
        (Scp.send ssh_with_user ~recurse:true ~locals:[Fpath.v "bar"]
           ~remote:"docs/bar") ;
      [%expect
        {|
        RUN scp
        - arg: -q
        - arg: -B
        - arg: -r
        - arg: bar
        - arg: piers@spikemuth:docs/bar |}]

    let%expect_test "single receive with user" =
      prerr
        (Scp.receive ssh_with_user ~recurse:false ~remotes:["foo"]
           ~local:(Fpath.v "docs/foo")) ;
      [%expect
        {|
        RUN scp
        - arg: -q
        - arg: -B
        - arg: piers@spikemuth:foo
        - arg: docs/foo |}]

    let%expect_test "multiple receive with user" =
      prerr
        (Scp.receive ssh_with_user ~recurse:false
           ~remotes:["foo"; "bar"; "baz"] ~local:(Fpath.v "docs/")) ;
      [%expect
        {|
          RUN scp
          - arg: -q
          - arg: -B
          - arg: piers@spikemuth:foo
          - arg: piers@spikemuth:bar
          - arg: piers@spikemuth:baz
          - arg: docs/ |}]

    let%expect_test "recursive receive with user" =
      prerr
        (Scp.receive ssh_with_user ~recurse:true ~remotes:["bar"]
           ~local:(Fpath.v "docs/bar")) ;
      [%expect
        {|
        RUN scp
        - arg: -q
        - arg: -B
        - arg: -r
        - arg: piers@spikemuth:bar
        - arg: docs/bar |}]
  end )
