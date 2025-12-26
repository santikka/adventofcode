# Data
data <- readLines("2024/inputs/day14_input.txt")
match <- gregexec("\\-{0,1}[0-9]+", data)
nums <- regmatches(data, match) |>
  lapply(as.integer) |>
  do.call(what = "rbind")
pos <- nums[, 1:2]
velo <- nums[, 3:4]

# Part 1
security_factor <- function(secs, rows, cols, pos, velo) {
  for (i in seq_len(secs)) {
    pos <- pos + velo
    pos[, 1] <- pos[, 1] %% cols
    pos[, 2] <- pos[, 2] %% rows
  }
  n <- rows %/% 2
  m <- cols %/% 2
  print(pos)
  q1 <- sum(pos[, 1] > m & pos[, 2] > n)
  q2 <- sum(pos[, 1] < m & pos[, 2] > n)
  q3 <- sum(pos[, 1] < m & pos[, 2] < n)
  q4 <- sum(pos[, 1] > m & pos[, 2] < n)
  q1 * q2 * q3 * q4
}

result1 <- security_factor(secs = 100, rows = 103 , cols = 101, pos, velo)
result1

# Part 2
find_unique <- function(rows, cols, pos, velo) {
  i <- 0
  while (any(duplicated(pos))) {
    i <- i + 1
    pos <- pos + velo
    pos[, 1] <- pos[, 1] %% cols
    pos[, 2] <- pos[, 2] %% rows
  }
  plot_data <- pos
  plot_data[, 2] <- rows - pos[, 2] - 1
  plot(plot_data, pch = 19)
  return(i)
}

result2 <- find_unique(rows = 103 , cols = 101, pos, velo)
result2

