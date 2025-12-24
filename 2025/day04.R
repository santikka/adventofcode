# Data
data <- readLines("2025/inputs/day04_input.txt") |>
  strsplit(split = "")
rolls <- do.call("rbind", data)

# Part 1
idx_mat <- as.matrix(
  expand.grid(i = c(0, -1, 1), j = c(0, -1, 1))[-1, ]
)
result1 <- 0
n <- nrow(rolls)
p <- ncol(rolls)
for (i in 1:n) {
  for (j in 1:p) {
    idx <- idx_mat + cbind(rep(i, 8), rep(j, 8))
    bounds <- (idx[, 1] > 0) &
      (idx[, 1] <= n) &
      (idx[, 2] > 0) &
      (idx[, 2] <= p)
    idx <- idx[bounds, ]
    result1 <- result1 + 1 *
      (sum(rolls[idx] == "@") < 4) * (rolls[i, j] == "@")
  }
}
result1

# Part 2
result2 <- 0
rolls_now <- rolls
any_removed <- TRUE
while (any_removed) {
  any_removed <- FALSE
  for (i in 1:n) {
    for (j in 1:p) {
      if (rolls_now[i, j] != "@") next
      idx <- idx_mat + cbind(rep(i, 8), rep(j, 8))
      bounds <- (idx[, 1] > 0) &
        (idx[, 1] <= n) &
        (idx[, 2] > 0) &
        (idx[, 2] <= p)
      idx <- idx[bounds, ]
      remove <- (sum(rolls_now[idx] == "@") < 4)
      if (remove) {
        any_removed <- TRUE
        rolls_now[i, j] <- "."
        result2 <- result2 + 1
      }
    }
  }
}
result2
