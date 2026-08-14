# Compatibility entry point.
#
# The authoritative shared implementation lives under shared/ so the player
# folder remains self-contained when it is distributed to friends. The control
# app sources the same file instead of carrying a second copy.
source("shared/global_core.R", local = FALSE)

