player_dir <- "DND APP Drachuri Edition Player_v2"
files <- c(file.path(player_dir, "server.R"), list.files(
  file.path(player_dir, "server"), pattern = "[.]R$", full.names = TRUE
))
files <- files[!grepl("(_old|_backup|_new|_UPDATED|_sad)[.]R$", files)]

count <- function(pattern, lines) sum(grepl(pattern, lines, perl = TRUE))
rows <- lapply(files, function(path) {
  lines <- readLines(path, warn = FALSE)
  data.frame(
    file = path,
    observers = count("\\bobserve(Event)?\\s*\\(", lines),
    reactives = count("\\breactive\\s*\\(", lines),
    renders = count("\\brender[A-Za-z]+\\s*\\(", lines),
    db_helper_calls = count("\\b(get|load|save|set|upsert|damage|heal)_[A-Za-z0-9_]+\\s*\\(", lines),
    lines = length(lines),
    stringsAsFactors = FALSE
  )
})

profile <- do.call(rbind, rows)
profile$score <- profile$observers + profile$reactives + profile$renders + profile$db_helper_calls
profile <- profile[order(-profile$score), ]
print(profile, row.names = FALSE)

cat("\nFor live snapshot timings, launch with DND_PROFILE=true.\n")
