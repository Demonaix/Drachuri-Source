app_dir <- normalizePath(getwd())
workspace_dir <- normalizePath(file.path(app_dir, ".."))
player_dir <- file.path(workspace_dir, "DND APP Drachuri Edition Player_v2")
launcher_dir <- file.path(app_dir, "launcher")
log_dir <- file.path(launcher_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, "launcher.log")

log_message <- function(...) {
  text <- paste0(..., collapse = "")
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

fail <- function(message, status = 1L) {
  log_message("ERROR: ", message)
  cat("\nDrachuri Control could not start.\n", message, "\n\n")
  cat("Diagnostic log: ", log_file, "\n", sep = "")
  quit(save = "no", status = status)
}

if (!file.exists(file.path(app_dir, "app.R"))) {
  fail("Run the launcher from inside the DND control folder.")
}
if (!file.exists(file.path(player_dir, "shared", "global_core.R")) ||
    !file.exists(file.path(player_dir, "shared", "session_db_core.R"))) {
  fail(paste(
    "The player folder must remain beside the control folder.",
    "Shared game and database code could not be found."
  ))
}

lock_file <- file.path(app_dir, "renv.lock")
if (!file.exists(lock_file)) fail("renv.lock is missing from the control folder.")

log_message("Starting Drachuri Control bootstrap")
log_message("R runtime: ", R.version.string)
log_message("Application: ", app_dir)

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

bootstrap_library <- file.path(launcher_dir, "bootstrap-library")
dir.create(bootstrap_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(bootstrap_library, .libPaths()))

if (!requireNamespace("renv", quietly = TRUE)) {
  log_message("Installing the dependency manager (one-time setup)...")
  tryCatch(
    install.packages("renv", lib = bootstrap_library, quiet = TRUE),
    error = function(e) fail(paste("Could not install renv:", conditionMessage(e)))
  )
}

project_library <- renv::paths$library(project = app_dir)
dir.create(project_library, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(RENV_PROJECT = app_dir)
.libPaths(c(project_library, bootstrap_library, .libPaths()))

lock_hash <- unname(tools::md5sum(lock_file))
marker_file <- file.path(launcher_dir, ".restored-lock-md5")
restored_hash <- if (file.exists(marker_file)) trimws(readLines(marker_file, warn = FALSE)[1L]) else ""
needs_restore <- !identical(lock_hash, restored_hash) || !dir.exists(project_library)

if (needs_restore) {
  log_message("Installing/updating required packages. This can take several minutes the first time...")
  tryCatch({
    renv::restore(project = app_dir, lockfile = lock_file, prompt = FALSE)
    log_message("Package restore completed successfully")
  }, error = function(e) {
    fail(paste(
      "Package installation failed.",
      conditionMessage(e),
      "Check your internet connection and the launcher log."
    ))
  })
} else {
  log_message("Dependencies already match this application version")
}

required_packages <- c("shiny", "shinyjs", "DBI", "RPostgres", "pool")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
if (length(missing_packages)) {
  fail(paste("Required packages are still missing:", paste(missing_packages, collapse = ", ")))
}

writeLines(lock_hash, marker_file, useBytes = TRUE)

if (identical(tolower(Sys.getenv("DND_LAUNCHER_CHECK_ONLY", "false")), "true")) {
  log_message("Launcher check completed; application launch skipped")
  quit(save = "no", status = 0L)
}

# Load connection settings before Shiny chooses the application layout. This
# keeps shortcut launches consistent with launches made from RStudio.
env_file <- file.path(app_dir, "env.R")
if (file.exists(env_file)) {
  tryCatch(
    sys.source(env_file, envir = globalenv()),
    error = function(e) fail(paste("Could not load env.R:", conditionMessage(e)))
  )
}

launch_browser <- !identical(tolower(Sys.getenv("DND_LAUNCH_BROWSER", "true")), "false")
launch_port <- suppressWarnings(as.integer(Sys.getenv("DND_LAUNCH_PORT", "")))
if (is.na(launch_port) || launch_port < 1L) launch_port <- NULL

log_message("Launching control dashboard in the default browser")
options(shiny.launch.browser = launch_browser)

tryCatch(
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch_browser,
    port = launch_port
  ),
  error = function(e) fail(paste("The control app stopped with an error:", conditionMessage(e)))
)
