app_dir <- normalizePath(Sys.getenv("DRACHURI_BUILD_APP"), mustWork = TRUE)
library_dir <- normalizePath(Sys.getenv("DRACHURI_BUILD_LIBRARY"), mustWork = FALSE)
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library_dir, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Windows releases use CRAN's precompiled packages. Restoring historical versions
# from renv.lock can force packages such as rlang and glue to compile from source,
# which would make every release machine require a matching Rtools installation.
required <- c(
  "shiny", "shinyjs", "colourpicker", "dplyr", "jsonlite",
  "DBI", "RPostgres", "pool"
)
install.packages(
  required,
  lib = library_dir,
  dependencies = c("Depends", "Imports", "LinkingTo"),
  type = "binary"
)

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Windows package restore is incomplete: ", paste(missing, collapse = ", "))
cat("Windows dependency library is ready at", library_dir, "\n")
