# Fuzzer running logic

This library contains the top-level runners for the ACT fuzzer, as well
as things specific to those runners (such as action pools).

There are currently two fuzzer runners:

- the `Randomised` runner, which picks random actions;
- the `Replay` runner, which picks actions listed in a fuzzer trace.