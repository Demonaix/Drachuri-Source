# calls/sidebar_calls.R
sidebarTabServer(
  "sidebar",
  state      = core$state,
  restoring  = core$restoring,
  char_rev   = core$char_rev,
  add_log    = core$add_log
)