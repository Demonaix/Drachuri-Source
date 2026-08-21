#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", script_arg[[1]])
project_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
launcher_library <- file.path(
  project_root, "DND APP Drachuri Edition Player_v2", "launcher", "bootstrap-library"
)
if (dir.exists(launcher_library)) .libPaths(c(launcher_library, .libPaths()))
source(file.path(project_root, "DND APP Drachuri Edition 2 Control", "env.R"), local = TRUE)

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("SUPABASE_HOST"),
  port = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname = Sys.getenv("SUPABASE_DBNAME"),
  user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = "require"
)
on.exit(dbDisconnect(con), add = TRUE)

tables <- c(
  "character_blobs", "session_players", "weapons", "armour", "items",
  "Materials", "Condition"
)

columns <- dbGetQuery(
  con,
  paste(
    "SELECT table_name, column_name, data_type, is_nullable",
    "FROM information_schema.columns",
    "WHERE table_schema = $1",
    "ORDER BY table_name, ordinal_position"
  ),
  params = list("public")
)
columns <- columns[columns$table_name %in% tables, , drop = FALSE]
print(columns, row.names = FALSE)

cat("\nROW COUNTS\n")
for (table_name in tables) {
  if (!dbExistsTable(con, Id(schema = "public", table = table_name))) next
  count <- dbGetQuery(
    con,
    paste0("SELECT count(*) AS n FROM ", dbQuoteIdentifier(con, Id(schema = "public", table = table_name)))
  )
  cat(sprintf("%-22s %s\n", table_name, count$n[[1]]))
}

cat("\nCONSTRAINTS\n")
constraints <- dbGetQuery(
  con,
  paste(
    "SELECT tc.table_name, tc.constraint_name, tc.constraint_type,",
    "       kcu.column_name, ccu.table_name AS referenced_table,",
    "       ccu.column_name AS referenced_column",
    "FROM information_schema.table_constraints tc",
    "LEFT JOIN information_schema.key_column_usage kcu",
    "  ON tc.constraint_schema = kcu.constraint_schema",
    " AND tc.constraint_name = kcu.constraint_name",
    "LEFT JOIN information_schema.constraint_column_usage ccu",
    "  ON tc.constraint_schema = ccu.constraint_schema",
    " AND tc.constraint_name = ccu.constraint_name",
    "WHERE tc.table_schema = $1",
    "ORDER BY tc.table_name, tc.constraint_name, kcu.ordinal_position"
  ),
  params = list("public")
)
constraints <- constraints[constraints$table_name %in% tables, , drop = FALSE]
print(constraints, row.names = FALSE)

for (definition_table in c("item_materials", "item_conditions")) {
  if (dbExistsTable(con, definition_table)) {
    cat("\n", toupper(definition_table), "\n", sep = "")
    print(dbReadTable(con, definition_table), row.names = FALSE)
  }
}

cat("\nCHARACTER BLOB SIZES (no blob contents printed)\n")
if (dbExistsTable(con, Id(schema = "public", table = "character_blobs"))) {
  blob_columns <- columns$column_name[columns$table_name == "character_blobs"]
  size_column <- intersect(c("state_blob", "blob", "character_blob"), blob_columns)
  if (length(size_column)) {
    id_column <- if ("id" %in% blob_columns) "id" else blob_columns[[1]]
    size_sql <- paste0(
      "SELECT ", dbQuoteIdentifier(con, id_column), " AS character_id, ",
      "octet_length(", dbQuoteIdentifier(con, size_column[[1]]), ") AS bytes ",
      "FROM ", dbQuoteIdentifier(con, Id(schema = "public", table = "character_blobs")),
      " ORDER BY bytes DESC"
    )
    print(dbGetQuery(con, size_sql), row.names = FALSE)
  }
}

cat("\nOWNED INVENTORY SUMMARY\n")
blob_rows <- dbGetQuery(
  con,
  "SELECT id, char_name, state_blob FROM public.character_blobs ORDER BY char_name, id"
)
for (i in seq_len(nrow(blob_rows))) {
  character <- tryCatch(unserialize(blob_rows$state_blob[[i]]), error = function(e) NULL)
  inventory <- character$inventory$items
  gold <- suppressWarnings(as.numeric(character$inventory$gold %||% 0))
  if (is.null(inventory) || !is.data.frame(inventory)) inventory <- data.frame()
  cat(sprintf(
    "\n%s [%s] — %d item row(s), %s gold\n",
    blob_rows$char_name[[i]], blob_rows$id[[i]], nrow(inventory), gold
  ))
  if (nrow(inventory)) {
    safe <- data.frame(
      id = as.character(inventory$id %||% ""),
      name = as.character(inventory$name %||% ""),
      type = as.character(inventory$type %||% "item"),
      qty = suppressWarnings(as.numeric(inventory$qty %||% 1)),
      equipped = as.logical(inventory$equipped %||% FALSE),
      stringsAsFactors = FALSE
    )
    print(safe, row.names = FALSE)
  }
}
