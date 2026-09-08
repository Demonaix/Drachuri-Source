args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) quit(save = "no", status = 0L)

product <- args[[1L]]
platform <- args[[2L]]
current_version <- trimws(args[[3L]])
repository_file <- args[[4L]]
repository <- if (file.exists(repository_file)) trimws(readLines(repository_file, warn = FALSE)[1L]) else ""
if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) quit(save = "no", status = 0L)
if (!requireNamespace("jsonlite", quietly = TRUE)) quit(save = "no", status = 0L)

parse_version <- function(x) {
  bits <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+){1,3}", x, perl = TRUE))
  if (!length(bits) || !nzchar(bits)) bits <- "0.0.0"
  base::numeric_version(bits)
}

manifest_url <- sprintf("https://github.com/%s/releases/latest/download/drachuri-update.json", repository)
manifest <- tryCatch({
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 5)
  jsonlite::fromJSON(manifest_url, simplifyVector = FALSE)
}, error = function(e) NULL)
if (is.null(manifest) || is.null(manifest$version) ||
    parse_version(manifest$version) <= parse_version(current_version)) {
  quit(save = "no", status = 0L)
}

asset <- manifest$products[[product]][[platform]]
if (is.null(asset) || !is.character(asset$url) || !nzchar(asset$url)) quit(save = "no", status = 0L)
notes <- if (is.character(manifest$notes) && nzchar(manifest$notes)) manifest$notes else "A new Drachuri update is ready."
message <- sprintf("Version %s is available. You currently have %s.\n\n%s", manifest$version, current_version, notes)

download <- FALSE
if (identical(platform, "mac")) {
  script <- paste(
    'on run argv',
    'display dialog (item 1 of argv) buttons {"Later", "Download & Open"} default button "Download & Open" with title "Drachuri Update"',
    'return button returned of result',
    'end run', sep = "\n"
  )
  answer <- tryCatch(
    system2("osascript", c("-e", shQuote(script), "--", shQuote(message)), stdout = TRUE, stderr = FALSE),
    error = function(e) ""
  )
  download <- any(grepl("Download & Open", answer, fixed = TRUE))
} else if (.Platform$OS.type == "windows") {
  download <- identical(utils::winDialog("yesno", paste0(message, "\n\nDownload it now?")), "YES")
}

if (download) {
  download_dir <- path.expand("~/Downloads")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- utils::URLdecode(basename(sub("\\?.*$", "", asset$url)))
  destination <- file.path(download_dir, file_name)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout))
  downloaded <- isTRUE(tryCatch({
    utils::download.file(asset$url, destination, mode = "wb", quiet = FALSE)
    TRUE
  }, error = function(e) FALSE))
  expected_hash <- if (is.character(asset$sha256) && length(asset$sha256)) {
    tolower(asset$sha256[[1L]])
  } else ""
  verified <- downloaded
  if (downloaded && nzchar(expected_hash)) {
    if (identical(platform, "mac")) {
      hash_output <- tryCatch(
        system2("/usr/bin/shasum", c("-a", "256", shQuote(destination)), stdout = TRUE, stderr = TRUE),
        error = function(e) character()
      )
      actual_hash <- if (length(hash_output)) strsplit(trimws(hash_output[[1L]]), "[[:space:]]+")[[1L]][[1L]] else ""
    } else {
      actual_hash <- if (requireNamespace("openssl", quietly = TRUE)) {
        paste(format(openssl::sha256(file(destination))), collapse = "")
      } else ""
    }
    verified <- nzchar(actual_hash) && identical(tolower(actual_hash), expected_hash)
  }
  if (!verified) {
    if (file.exists(destination)) unlink(destination)
    warning_message <- "The Drachuri update could not be downloaded or verified. Please try again later."
    if (identical(platform, "mac")) {
      alert <- paste('display alert "Drachuri Update" message', shQuote(warning_message), 'as critical')
      try(system2("osascript", c("-e", shQuote(alert)), stdout = FALSE, stderr = FALSE), silent = TRUE)
    } else if (.Platform$OS.type == "windows") {
      utils::winDialog("ok", warning_message)
    }
  } else if (identical(platform, "mac")) {
    system2("open", shQuote(destination), wait = FALSE)
  } else if (.Platform$OS.type == "windows") {
    shell.exec(destination)
  }
}
quit(save = "no", status = 0L)
