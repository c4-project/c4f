(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fairly basic wrapper over the system's scp binary.

    This differs from the implementation in Jane Street's Shell module by
    supporting both send and receive, and multi-file transfer. On the flip
    side, it is significantly less well-tested and likely to eat your
    laundry. *)

(** {1 Normal SCP}

    The module itself implements {!Scp_types.S}, and this implementation is
    probably what anything other than test code will need. *)

include Scp_types.S

(** {1 SCP over a particular runner}

    We provide a functor for instantiating SCP support where the scp binary
    is run using a particular {!Runner_types.S}. Using {!Runner.Local} will
    give the same result as just using the implementation above.

    This functor mainly exists to allow use of mock runners for testing. *)

module Using_runner (Runner : Runner_types.S) : Scp_types.S
