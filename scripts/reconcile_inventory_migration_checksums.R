#!/usr/bin/env Rscript

# One-time reconciliation after making migrations 007/008 fresh-install safe.
# Their live effects did not change; only CREATE-if-missing and adaptive ID-type
# handling were added. Refuse to run unless the expected live tables exist.

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~\\+~", " ", sub("^--file=", "", file_arg[[1L]]))
project_dir <- normalizePath(file.path(dirname(script_path), ".."))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")
launcher_library <- file.path(player_dir, "launcher", "bootstrap-library")
if (dir.exists(launcher_library)) .libPaths(c(launcher_library, .libPaths()))
sys.source(file.path(player_dir, "env.R"), envir = globalenv())

suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
con <- dbConnect(
  RPostgres::Postgres(), host=Sys.getenv("SUPABASE_HOST"),
  port=as.integer(Sys.getenv("SUPABASE_PORT")), dbname=Sys.getenv("SUPABASE_DBNAME"),
  user=Sys.getenv("SUPABASE_USER"), password=Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode=Sys.getenv("SUPABASE_SSLMODE", unset="require")
)
on.exit(dbDisconnect(con), add=TRUE)

required <- c("character_inventory_items", "character_wallets", "item_materials",
              "item_conditions", "equipment_assignment_log")
present <- vapply(required, function(x) dbExistsTable(con, x), logical(1))
if (!all(present)) stop("Refusing checksum reconciliation; missing: ", paste(required[!present], collapse=", "))

versions <- c("007", "008")
paths <- file.path(project_dir, "database", "migrations", c(
  "007_relational_inventory.sql", "008_equipment_provenance.sql"
))
checksums <- unname(tools::md5sum(paths))

dbWithTransaction(con, {
  for (i in seq_along(versions)) {
    changed <- dbExecute(con,
      "UPDATE schema_migrations SET checksum=$2 WHERE version=$1",
      params=list(versions[[i]], checksums[[i]]))
    if (changed != 1L) stop("Migration ledger does not contain version ", versions[[i]])
  }
})
cat("Reconciled migration checksums for 007 and 008 after fresh-install compatibility correction.\n")
