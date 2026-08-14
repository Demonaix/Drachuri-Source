args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
  stop("Usage: Rscript scripts/deduplicate_r_functions.R FILE [FILE ...]")
}

for (path in args) {
  expressions <- parse(path, keep.source = TRUE)
  source_refs <- attr(expressions, "srcref")
  definitions <- list()

  for (i in seq_along(expressions)) {
    expr <- expressions[[i]]
    is_function_definition <-
      is.call(expr) &&
      identical(expr[[1L]], as.name("<-")) &&
      is.symbol(expr[[2L]]) &&
      is.call(expr[[3L]]) &&
      identical(expr[[3L]][[1L]], as.name("function"))

    if (is_function_definition) {
      name <- as.character(expr[[2L]])
      definitions[[name]] <- c(definitions[[name]], i)
    }
  }

  duplicate_names <- names(definitions)[lengths(definitions) > 1L]
  if (length(duplicate_names) == 0L) {
    message(path, ": no duplicate top-level functions")
    next
  }

  remove_expression <- integer()
  for (name in duplicate_names) {
    indices <- definitions[[name]]
    # R uses the last assignment, so retain the last definition to preserve
    # the behaviour of the original source file.
    remove_expression <- c(remove_expression, head(indices, -1L))
  }

  remove_lines <- integer()
  for (i in remove_expression) {
    ref <- source_refs[[i]]
    remove_lines <- c(remove_lines, seq.int(ref[[1L]], ref[[3L]]))
  }

  lines <- readLines(path, warn = FALSE)
  rewritten <- lines[setdiff(seq_along(lines), unique(remove_lines))]
  temp_path <- paste0(path, ".deduplicate.tmp")
  writeLines(rewritten, temp_path, useBytes = TRUE)
  parse(temp_path)

  if (!file.rename(temp_path, path)) {
    unlink(temp_path)
    stop("Could not replace ", path)
  }

  message(
    path,
    ": removed ", length(remove_expression),
    " shadowed function definition(s): ",
    paste(duplicate_names, collapse = ", ")
  )
}
