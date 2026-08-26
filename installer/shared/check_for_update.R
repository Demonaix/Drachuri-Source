args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) quit(save = "no", status = 0L)

product <- args[[1L]]
platform <- args[[2L]]
current_version <- trimws(args[[3L]])
repository_file <- args[[4L]]
repository <- if (file.exists(repository_file)) trimws(readLines(repository_file, warn = FALSE)[1L]) else ""
if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) quit(save = "no", status = 0L)
if (!requireNamespace("jsonlite", quietly = TRUE)) quit(save = "no", status = 0L)

numeric_version <- function(x) {
  bits <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+){1,3}", x, perl = TRUE))
  if (!length(bits) || !nzchar(bits)) return(numeric_version("0.0.0"))
  utils::numeric_version(bits)
}

manifest_url <- sprintf("https://github.com/%s/releases/latest/download/drachuri-update.json", repository)
manifest <- tryCatch({
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 5)
  jsonlite::fromJSON(manifest_url, simplifyVector = FALSE)
}, error = function(e) NULL)
if (is.null(manifest) || is.null(manifest$version) ||
    numeric_version(manifest$version) <= numeric_version(current_version)) {
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
    'display dialog (item 1 of argv) buttons {"Later", "Download Update"} default button "Download Update" with title "Drachuri Update"',
    'return button returned of result',
    'end run', sep = "\n"
  )
  answer <- tryCatch(system2("osascript", c("-e", shQuote(script), shQuote(message)), stdout = TRUE, stderr = FALSE), error = function(e) "")
  download <- any(grepl("Download Update", answer, fixed = TRUE))
} else if (.Platform$OS.type == "windows") {
  download <- identical(utils::winDialog("yesno", paste0(message, "\n\nDownload it now?")), "YES")
}

if (download) utils::browseURL(asset$url)
quit(save = "no", status = 0L)
