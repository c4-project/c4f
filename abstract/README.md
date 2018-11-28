# `abstract`: the abstract program model

This part of `act` contains an abstracted, idealised tree structure
representing assembly programs in an architecture-independent way.
It is used by the rest of `act` to perform analysis on a program
without depending on the intricacies of the language itself.

`abstract` sits below `lib`, but above `utils`, in the `act`
hierarchy.  Since it doesn't do anything platform-specific, it depends
on `core_kernel`, not `core` (though `utils` itself currently depends
on `core`).  Future work may move this to `base`.