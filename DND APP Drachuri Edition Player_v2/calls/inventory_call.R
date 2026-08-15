inventoryTabServer(
  "inventory",
  state = core$state,
  restoring = core$restoring,
  add_log = core$add_log,
  char_rev = core$char_rev,
  session_id = reactive(core$state$active_session_id),
  character_id = reactive(core$state$char_id)
)
