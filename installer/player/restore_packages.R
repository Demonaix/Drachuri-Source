app_dir <- normalizePath(Sys.getenv("DRACHURI_BUILD_APP"), mustWork = TRUE)
library_dir <- normalizePath(Sys.getenv("DRACHURI_BUILD_LIBRARY"), mustWork = FALSE)
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library_dir, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", lib = library_dir)
renv::restore(project = app_dir, library = library_dir, lockfile = file.path(app_dir, "renv.lock"), prompt = FALSE)

required <- c("shiny", "shinyjs", "dplyr", "jsonlite", "DBI", "RPostgres", "pool")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Windows package restore is incomplete: ", paste(missing, collapse = ", "))
cat("Windows dependency library is ready at", library_dir, "\n")
