args <- commandArgs(trailingOnly = TRUE)

usage <- function(status = 0L) {
  cat(
    "Usage: Rscript scripts/migrate_database.R [--status | --dry-run | --baseline]\n",
    "\n",
    "  no option   Apply pending migrations.\n",
    "  --status    Read-only report of applied and pending migrations.\n",
    "  --dry-run   Read-only list of migrations that would be applied.\n",
    "  --baseline  Validate an existing schema and record current migrations\n",
    "              without executing their SQL.\n",
    sep = ""
  )
  quit(save = "no", status = status)
}

if (any(args %in% c("--help", "-h"))) usage()
valid_args <- c("--status", "--dry-run", "--baseline")
if (length(args) > 1L || (length(args) == 1L && !args[[1L]] %in% valid_args)) usage(2L)
mode <- if (length(args)) sub("^--", "", args[[1L]]) else "apply"

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else "scripts/migrate_database.R"
# R 4.2's Rscript launcher encodes spaces in --file paths as ~+~.
script_path <- gsub("~\\+~", " ", script_path)
project_dir <- normalizePath(file.path(dirname(script_path), ".."))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")
migrations_dir <- file.path(project_dir, "database", "migrations")

# Reuse packages installed by the one-click launcher when they are not in the
# machine-wide R library.
launcher_library <- file.path(player_dir, "launcher", "bootstrap-library")
if (dir.exists(launcher_library)) .libPaths(c(launcher_library, .libPaths()))

if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RPostgres", quietly = TRUE)) {
  stop("DBI and RPostgres are required. Run the player launcher once first.", call. = FALSE)
}

# env.R only fills missing values, so test and CI environments can safely
# provide their own isolated PostgreSQL settings.
env_file <- file.path(player_dir, "env.R")
if (file.exists(env_file)) sys.source(env_file, envir = globalenv())

db_port <- suppressWarnings(as.integer(Sys.getenv("SUPABASE_PORT", unset = "5432")))
if (is.na(db_port) || db_port < 1L || db_port > 65535L) stop("Invalid SUPABASE_PORT", call. = FALSE)

required_settings <- c("SUPABASE_HOST", "SUPABASE_DBNAME", "SUPABASE_USER", "SUPABASE_DB_PASSWORD")
missing_settings <- required_settings[!nzchar(Sys.getenv(required_settings, unset = ""))]
if (length(missing_settings)) {
  stop("Missing database settings: ", paste(missing_settings, collapse = ", "), call. = FALSE)
}

migration_paths <- sort(list.files(
  migrations_dir,
  pattern = "^[0-9]{3}_[A-Za-z0-9_-]+\\.sql$",
  full.names = TRUE
))
if (!length(migration_paths)) stop("No migration files found in ", migrations_dir, call. = FALSE)

migrations <- data.frame(
  version = sub("_.*$", "", basename(migration_paths)),
  name = sub("\\.sql$", "", sub("^[0-9]{3}_", "", basename(migration_paths))),
  path = migration_paths,
  checksum = unname(tools::md5sum(migration_paths)),
  stringsAsFactors = FALSE
)
if (anyDuplicated(migrations$version)) stop("Migration versions must be unique", call. = FALSE)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("SUPABASE_HOST"),
  port = db_port,
  dbname = Sys.getenv("SUPABASE_DBNAME"),
  user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = Sys.getenv("SUPABASE_SSLMODE", unset = "require")
)
on.exit(DBI::dbDisconnect(con), add = TRUE)

ledger_exists <- isTRUE(DBI::dbGetQuery(
  con,
  "SELECT to_regclass('public.schema_migrations') IS NOT NULL AS present"
)$present[[1L]])

read_ledger <- function() {
  if (!ledger_exists) {
    return(data.frame(version = character(), name = character(), checksum = character()))
  }
  DBI::dbGetQuery(
    con,
    "SELECT version, name, checksum, applied_at FROM schema_migrations ORDER BY version"
  )
}

applied <- read_ledger()
if (nrow(applied)) {
  known <- merge(applied, migrations, by = "version", suffixes = c("_applied", "_file"))
  changed <- known$checksum_applied != known$checksum_file
  if (any(changed)) {
    stop(
      "Applied migration files were modified: ",
      paste(known$version[changed], collapse = ", "),
      ". Add a new migration instead.",
      call. = FALSE
    )
  }
}

pending <- migrations[!migrations$version %in% applied$version, , drop = FALSE]

if (mode == "status") {
  cat("Database: ", Sys.getenv("SUPABASE_DBNAME"), " at ", Sys.getenv("SUPABASE_HOST"), "\n", sep = "")
  if (nrow(applied)) {
    for (i in seq_len(nrow(applied))) cat("APPLIED ", applied$version[[i]], " ", applied$name[[i]], "\n", sep = "")
  }
  if (nrow(pending)) {
    for (i in seq_len(nrow(pending))) cat("PENDING ", pending$version[[i]], " ", pending$name[[i]], "\n", sep = "")
  }
  if (!nrow(pending)) cat("Schema is up to date.\n")
  quit(save = "no", status = 0L)
}

if (mode == "dry-run") {
  if (!nrow(pending)) {
    cat("No pending migrations.\n")
  } else {
    cat("Would apply:\n")
    for (i in seq_len(nrow(pending))) cat("  ", pending$version[[i]], " ", pending$name[[i]], "\n", sep = "")
  }
  quit(save = "no", status = 0L)
}

invisible(DBI::dbExecute(con, "SELECT pg_advisory_lock(hashtext('drachuri_schema_migrations'))"))
on.exit(try(DBI::dbExecute(con, "SELECT pg_advisory_unlock(hashtext('drachuri_schema_migrations'))"), silent = TRUE), add = TRUE)

invisible(DBI::dbExecute(
  con,
  paste(
    "CREATE TABLE IF NOT EXISTS schema_migrations (",
    "version TEXT PRIMARY KEY,",
    "name TEXT NOT NULL,",
    "checksum TEXT NOT NULL,",
    "applied_at TIMESTAMPTZ NOT NULL DEFAULT now()",
    ")"
  )
))
ledger_exists <- TRUE
applied <- read_ledger()
pending <- migrations[!migrations$version %in% applied$version, , drop = FALSE]

record_migration <- function(row) {
  DBI::dbExecute(
    con,
    "INSERT INTO schema_migrations (version, name, checksum) VALUES ($1, $2, $3)",
    params = list(row$version[[1L]], row$name[[1L]], row$checksum[[1L]])
  )
}

if (mode == "baseline") {
  required_tables <- c(
    "character_blobs", "maps", "map_tiles", "game_sessions", "session_players",
    "encounters", "npc_templates", "encounter_enemies", "encounter_positions",
    "combat_state", "game_events", "player_positions", "enemy_positions"
  )
  existing <- DBI::dbGetQuery(
    con,
    "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"
  )$table_name
  missing_tables <- setdiff(required_tables, existing)
  if (length(missing_tables)) {
    stop("Cannot baseline; required tables are missing: ", paste(missing_tables, collapse = ", "), call. = FALSE)
  }
  if (nrow(pending)) {
    DBI::dbWithTransaction(con, {
      for (i in seq_len(nrow(pending))) record_migration(pending[i, , drop = FALSE])
    })
  }
  cat("Baseline recorded after validating ", length(required_tables), " required tables.\n", sep = "")
  quit(save = "no", status = 0L)
}

if (!nrow(pending)) {
  cat("No pending migrations.\n")
  quit(save = "no", status = 0L)
}

for (i in seq_len(nrow(pending))) {
  row <- pending[i, , drop = FALSE]
  sql_lines <- readLines(row$path[[1L]], warn = FALSE)
  # The original baseline was a standalone psql script. Transactions are now
  # owned by the runner so each migration and ledger entry commit atomically.
  sql_lines <- sql_lines[!toupper(trimws(sql_lines)) %in% c("BEGIN;", "COMMIT;")]
  sql <- paste(sql_lines, collapse = "\n")
  cat("Applying ", row$version[[1L]], " ", row$name[[1L]], "...\n", sep = "")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, sql, immediate = TRUE)
    record_migration(row)
  })
}

cat("Applied ", nrow(pending), " migration(s). Schema is up to date.\n", sep = "")
