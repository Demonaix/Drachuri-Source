app_dir <- normalizePath(Sys.getenv("DRACHURI_APP_DIR"), mustWork = TRUE)
install_dir <- normalizePath(Sys.getenv("DRACHURI_INSTALL_DIR"), mustWork = TRUE)
log_dir <- Sys.getenv("DRACHURI_LOG_DIR", file.path(install_dir, "logs"))
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, "player.log")

log_message <- function(...) {
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(..., collapse = ""))
  cat(line, "\n", file = log_file, append = TRUE)
}

fail <- function(message) {
  log_message("ERROR: ", message)
  if (.Platform$OS.type == "windows") {
    utils::winDialog("ok", paste("Drachuri Player could not start.\n\n", message,
                                  "\n\nDiagnostic log:\n", log_file))
  }
  quit(save = "no", status = 1L)
}

setwd(app_dir)
log_message("Starting installed Drachuri Player from ", app_dir)
required <- c("shiny", "shinyjs", "dplyr", "jsonlite", "DBI", "RPostgres", "pool")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) fail(paste("The installation is missing:", paste(missing, collapse = ", "),
                                "Run the installer again to repair it."))

tryCatch({
  options(shiny.launch.browser = TRUE)
  shiny::runApp(appDir = app_dir, launch.browser = TRUE)
}, error = function(e) fail(conditionMessage(e)))
