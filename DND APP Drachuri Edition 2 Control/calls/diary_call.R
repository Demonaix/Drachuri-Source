# calls/diary_call.R
diaryTabServer(
  id        = "diary",          # MUST match diaryTabUI("diary")
  state     = core$state,
  restoring = core$restoring,
  add_log   = core$add_log,
  char_rev  = core$char_rev
)