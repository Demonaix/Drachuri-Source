args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript scripts/remove_r_functions.R FILE FUNCTION [FUNCTION ...]")
}

path <- args[[1L]]
targets <- args[-1L]
expressions <- parse(path, keep.source = TRUE)
source_refs <- attr(expressions, "srcref")
remove_lines <- integer()

for (i in seq_along(expressions)) {
  expr <- expressions[[i]]
  is_target <-
    is.call(expr) &&
    identical(expr[[1L]], as.name("<-")) &&
    is.symbol(expr[[2L]]) &&
    as.character(expr[[2L]]) %in% targets &&
    is.call(expr[[3L]]) &&
    identical(expr[[3L]][[1L]], as.name("function"))

  if (is_target) {
    ref <- source_refs[[i]]
    remove_lines <- c(remove_lines, seq.int(ref[[1L]], ref[[3L]]))
  }
}

if (length(remove_lines) == 0L) stop("No matching function definitions found")

lines <- readLines(path, warn = FALSE)
rewritten <- lines[setdiff(seq_along(lines), unique(remove_lines))]
temp_path <- paste0(path, ".remove.tmp")
writeLines(rewritten, temp_path, useBytes = TRUE)
parse(temp_path)

if (!file.rename(temp_path, path)) {
  unlink(temp_path)
  stop("Could not replace ", path)
}

message(path, ": removed definitions for ", paste(targets, collapse = ", "))
