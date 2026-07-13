#!/usr/bin/env Rscript
# Syntax-check every R source file in the repo. This does not evaluate the code
# (no packages required) — it only confirms each file parses, catching the kind
# of syntax break that would otherwise only surface when the app is launched.

# all.files = FALSE (the default) already skips dot-dirs such as .git
files <- list.files(".", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

failed <- character(0)
for (f in files) {
  ok <- tryCatch(
    {
      parse(f)
      TRUE
    },
    error = function(e) {
      message("PARSE FAIL: ", f)
      message("  ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok) failed <- c(failed, f)
}

if (length(failed) > 0) {
  message("\n", length(failed), " file(s) failed to parse.")
  quit(status = 1)
}
cat("All", length(files), "R files parsed OK\n")
