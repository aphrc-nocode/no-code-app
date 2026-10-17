#### ---- Sign-in warm-up ---- ####

## Sourced once when the app starts, so this work is done before anyone signs in.

## Loaded on demand by the anonymization module during sign-in otherwise
## (sf also pulls in units, which reads its unit database on load)
invisible(requireNamespace("sf", quietly = TRUE))

## Byte-compiling the large module servers costs about 2 seconds on the first
## sign-in after every restart. The setup code runs once per session, so it is
## run uncompiled; the previous JIT level is restored when the caller returns,
## and the handlers the modules register are still compiled on first use.
pause_jit_until_return <- function(env = parent.frame()) {
  level <- compiler::enableJIT(0)
  do.call(on.exit, list(bquote(compiler::enableJIT(.(level))), add = TRUE), envir = env)
  invisible(level)
}
