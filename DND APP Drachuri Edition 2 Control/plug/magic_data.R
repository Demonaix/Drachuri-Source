# plug/magic_data.R
classification_table <- data.frame(
  ClassLevel = 0:6,
  RefillRate = c(0, 4, 6, 8, 10, 15, 20),
  SindreLevel = c(0,30, 50, 70, 90, 140, 180),
  MaxFlow = c(0, 20, 40, 60, 80, 120, 160),
  Locked = c(0, 40, 80,120, 160, 240, 480),
  Bound = c(0, 1, 2, 3, 4, 5, 6),
  stringsAsFactors = FALSE
)