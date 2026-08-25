app_dir <- normalizePath(Sys.getenv("DRACHURI_APP_DIR"), mustWork = TRUE)
install_dir <- normalizePath(Sys.getenv("DRACHURI_INSTALL_DIR"), mustWork = TRUE)
log_dir <- Sys.getenv("DRACHURI_LOG_DIR", file.path(install_dir, "logs"))
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, "player.log")
app_name <- Sys.getenv("DRACHURI_APP_NAME", "Drachuri Player")
log_connection <- file(log_file, open = "at", encoding = "UTF-8")
sink(log_connection, type = "output", append = TRUE)
sink(log_connection, type = "message", append = TRUE)

log_message <- function(...) {
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(..., collapse = ""))
  cat(line, "\n", file = log_file, append = TRUE)
}

fail <- function(message) {
  log_message("ERROR: ", message)
  if (.Platform$OS.type == "windows") {
    utils::winDialog("ok", paste(app_name, " could not start.\n\n", message,
                                  "\n\nDiagnostic log:\n", log_file))
  }
  quit(save = "no", status = 1L)
}

setwd(app_dir)
log_message("Starting installed ", app_name, " from ", app_dir)
required <- strsplit(Sys.getenv("DRACHURI_REQUIRED_PACKAGES", "shiny,shinyjs,dplyr,jsonlite,DBI,RPostgres,pool"), ",", fixed = TRUE)[[1L]]
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) fail(paste("The installation is missing:", paste(missing, collapse = ", "),
                                "Run the installer again to repair it."))

# Finder can launch immediately after login or network changes, before DNS is
# fully ready. Warm the exact Supabase route and record useful diagnostics.
if (file.exists(file.path(app_dir, "env.R"))) sys.source(file.path(app_dir, "env.R"), envir = globalenv())
db_ready <- FALSE
for (attempt in seq_len(3L)) {
  probe <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = Sys.getenv("SUPABASE_HOST"),
      port = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
      dbname = Sys.getenv("SUPABASE_DBNAME"), user = Sys.getenv("SUPABASE_USER"),
      password = Sys.getenv("SUPABASE_DB_PASSWORD"), sslmode = "require"
    ),
    error = function(e) { log_message("Database probe ", attempt, " failed: ", conditionMessage(e)); NULL }
  )
  if (!is.null(probe)) {
    db_ready <- isTRUE(tryCatch(DBI::dbGetQuery(probe, "SELECT 1 AS ok")$ok[[1L]] == 1L, error = function(e) FALSE))
    try(DBI::dbDisconnect(probe), silent = TRUE)
  }
  if (db_ready) break
  Sys.sleep(1)
}
log_message("UTF-8 locale: ", Sys.getlocale("LC_CTYPE"), "; database warm-up: ", if (db_ready) "online" else "offline")

tryCatch({
  options(shiny.launch.browser = TRUE)
  launch_port <- suppressWarnings(as.integer(Sys.getenv("DND_LAUNCH_PORT", "")))
  if (is.na(launch_port) || launch_port < 1L) launch_port <- NULL
  shiny::runApp(appDir = app_dir, launch.browser = TRUE, port = launch_port)
}, error = function(e) fail(conditionMessage(e)), finally = {
  pid_file <- Sys.getenv("DRACHURI_PID_FILE", "")
  if (nzchar(pid_file)) try(unlink(pid_file), silent = TRUE)
  try(sink(type = "message"), silent = TRUE)
  try(sink(type = "output"), silent = TRUE)
  try(close(log_connection), silent = TRUE)
})
