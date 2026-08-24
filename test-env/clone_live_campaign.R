args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else normalizePath(".", mustWork = TRUE)
pg_bin <- "/Library/PostgreSQL/18/bin"
required_tools <- file.path(pg_bin, c("pg_dump", "psql", "dropdb", "createdb"))
if (!all(file.exists(required_tools))) stop("PostgreSQL 18 command-line tools were not found under ", pg_bin, ".", call. = FALSE)

db_vars <- c("SUPABASE_HOST", "SUPABASE_PORT", "SUPABASE_DBNAME", "SUPABASE_USER", "SUPABASE_DB_PASSWORD", "SUPABASE_SSLMODE")
old_env <- Sys.getenv(db_vars, unset = NA_character_)
on.exit({
  Sys.unsetenv(db_vars)
  present <- !is.na(old_env)
  if (any(present)) do.call(Sys.setenv, as.list(stats::setNames(old_env[present], db_vars[present])))
}, add = TRUE)
Sys.unsetenv(db_vars)
source(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "env.R"), local = new.env(parent = baseenv()))
live <- Sys.getenv(db_vars, unset = "")
names(live) <- db_vars
if (any(!nzchar(live[c("SUPABASE_HOST", "SUPABASE_PORT", "SUPABASE_DBNAME", "SUPABASE_USER", "SUPABASE_DB_PASSWORD")]))) stop("The live Supabase connection settings are incomplete.", call. = FALSE)
if (live[["SUPABASE_DBNAME"]] == "drachuri_test" || grepl("drachuri-postgres|^/tmp/", live[["SUPABASE_HOST"]])) stop("Safety check failed: the source points at the local test database.", call. = FALSE)

snapshot_dir <- file.path(project_dir, "test-env", "snapshots")
dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
snapshot_path <- file.path(snapshot_dir, paste0("campaign-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".sql"))
pg_env <- paste0("PGPASSWORD=", live[["SUPABASE_DB_PASSWORD"]])
dump_args <- c(
  "--host", live[["SUPABASE_HOST"]], "--port", live[["SUPABASE_PORT"]],
  "--username", live[["SUPABASE_USER"]], "--dbname", live[["SUPABASE_DBNAME"]],
  "--schema=public", "--clean", "--if-exists", "--no-owner", "--no-acl",
  "--format=plain", "--file", shQuote(snapshot_path)
)
cat("Taking a read-only snapshot of the live public schema...\n")
status <- system2(file.path(pg_bin, "pg_dump"), dump_args, env = pg_env)
if (!identical(status, 0L) || !file.exists(snapshot_path) || file.info(snapshot_path)$size < 1000) {
  if (file.exists(snapshot_path)) unlink(snapshot_path)
  stop("The live snapshot failed. The local test database was not changed.", call. = FALSE)
}

start_status <- system2("sh", shQuote(file.path(project_dir, "test-env", "start_db.sh")))
if (!identical(start_status, 0L)) stop("The local PostgreSQL test server could not be started.", call. = FALSE)
local_host <- "/tmp/drachuri-postgres"; local_port <- "55432"; local_user <- "drachuri_test"; local_db <- "drachuri_test"
if (local_host != "/tmp/drachuri-postgres" || local_db != "drachuri_test") stop("Destination safety check failed.", call. = FALSE)
local_args <- c("-h", local_host, "-p", local_port, "-U", local_user)
terminate_sql <- shQuote("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='drachuri_test' AND pid<>pg_backend_pid();")
system2(file.path(pg_bin, "psql"), c(local_args, "-d", "postgres", "-c", terminate_sql), stdout = FALSE, stderr = FALSE)
if (!identical(system2(file.path(pg_bin, "dropdb"), c(local_args, "--if-exists", local_db)), 0L)) stop("Could not clear the local test database.", call. = FALSE)
if (!identical(system2(file.path(pg_bin, "createdb"), c(local_args, local_db)), 0L)) stop("Could not recreate the local test database.", call. = FALSE)
cat("Restoring the campaign snapshot into the isolated local test database...\n")
restore_status <- system2(file.path(pg_bin, "psql"), c(local_args, "--quiet", "-d", local_db, "--set=ON_ERROR_STOP=1", "--file", shQuote(snapshot_path)), stdout = FALSE)
if (!identical(restore_status, 0L)) stop("Snapshot restore failed; the local test database needs another refresh.", call. = FALSE)

check_sql <- paste(
  "SELECT (SELECT count(*) FROM character_blobs) AS characters,",
  "(SELECT count(*) FROM game_sessions) AS sessions,",
  "(SELECT count(*) FROM encounters) AS encounters;"
)
system2(file.path(pg_bin, "psql"), c(local_args, "-d", local_db, "-c", shQuote(check_sql)))
file.create(file.path(project_dir, "test-env", ".campaign-snapshot-loaded"))
cat("Campaign snapshot saved to: ", snapshot_path, "\n", sep = "")
cat("The local test apps will now retain this campaign copy until you refresh or run a resetting test.\n")
