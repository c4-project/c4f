# `Act_backend`: scaffolding for backends

In ACT terminology, a 'backend' is something that takes a Litmus test
(usually assembly) and produces either a set of states representing
the results of its possible executions, or a 'harness' that can be run
to get said states indirectly.

Backends are typically simulators (such as Herd7) or stress-testers
(such as Litmus7).

This module contains the typical array of signatures, functors, and
modules for building interfaces between ACT and backends, as well as a
notion of `Spec`s for backends, and a way to configure them that
ultimately gets exposed in the `act.conf` config file.