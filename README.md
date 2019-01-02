# contiguous-checked

A drop-in replacement for `contiguous` where all the functions
checks bounds at runtime. This is less efficient but gives
helpful error messages instead of segfaulting. It is intended
to be used when testing software.
