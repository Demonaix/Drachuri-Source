args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: prepare_mac_library.R APP_DIR OUTPUT_LIBRARY")
app_dir <- normalizePath(args[[1L]], mustWork = TRUE)
output <- normalizePath(args[[2L]], mustWork = FALSE)
dir.create(output, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("renv", quietly = TRUE)) stop("The existing launcher library does not contain renv.")
lock <- renv::lockfile_read(file.path(app_dir, "renv.lock"))
packages <- names(lock$Packages)
missing <- character()
for (package in packages) {
  source <- tryCatch(find.package(package), error = function(e) "")
  if (!nzchar(source)) { missing <- c(missing, package); next }
  destination <- file.path(output, package)
  if (dir.exists(destination)) unlink(destination, recursive = TRUE)
  if (!file.copy(source, output, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)) {
    stop("Could not copy package ", package)
  }
}
if (length(missing)) stop("Packages missing from this Mac: ", paste(missing, collapse = ", "))

.libPaths(c(output, .libPaths()))
required <- c("shiny", "shinyjs", "dplyr", "jsonlite", "DBI", "RPostgres", "pool")
unavailable <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(unavailable)) stop("Packaged Mac library is incomplete: ", paste(unavailable, collapse = ", "))
cat("Prepared", length(packages), "locked packages in", output, "\n")
