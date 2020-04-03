(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The raw output of a fuzzer runner. *)

(** Opaque type of fuzzer output, parametrised on the metadata attached. *)
type 'm t

(** {1 Constructing a fuzzer output} *)

(* TODO(@MattWindsor91): disambiguate output metadata from subject metadata. *)

val make : subject:Subject.Test.t -> metadata:'m -> 'm t
(** [make ~subject ~metadata] makes a fuzzer output with the given final
    [subject], var map [vars], and output metadata [metadata]. *)

(** {1 Extracting fields from a fuzzer output} *)

val subject : _ t -> Subject.Test.t
(** [subject out] gets the fuzzer test subject associated with [out]. *)

val metadata : 'm t -> 'm
(** [metadata out] gets the output-level metadata associated with [out].
    Generally, for random fuzzers this will be the trace, and for replaying
    fuzzers this will be empty. *)
