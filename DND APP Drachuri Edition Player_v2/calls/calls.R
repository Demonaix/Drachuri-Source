#Call server
callSkillsTab <- function(core) {
  skillsTabServer(
    id = "skills",
    state = core$state,
    restoring = core$restoring,
    add_log = core$add_log,
    char_rev = core$char_rev
  )
}