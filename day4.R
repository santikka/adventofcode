# Part 1
data <- readLines("inputs/day4_input.txt")
data <- strsplit(data, "")
rolls <- do.call("rbind", data)
idx_mat <- as.matrix(
  expand.grid(i = c(0, -1, 1), j = c(0, -1, 1))[-1, ]
)
result <- 0
n <- nrow(rolls)
p <- ncol(rolls)
for (i in 1:n) {
  for (j in 1:p) {
    idx <- idx_mat + cbind(rep(i, 8), rep(j, 8))
    bounds <- (idx[, 1] > 0) & (idx[, 1] <= n) & (idx[, 2] > 0) & (idx[, 2] <= p)
    idx <- idx[bounds, ]
    result <- result + 1 * (sum(rolls[idx] == "@") < 4) * (rolls[i, j] == "@")
  }
}
result

# Part 2
result <- 0
rolls_now <- rolls
any_removed <- TRUE
while (any_removed) {
  any_removed <- FALSE
  for (i in 1:n) {
    for (j in 1:p) {
      if (rolls_now[i, j] != "@") next
      idx <- idx_mat + cbind(rep(i, 8), rep(j, 8))
      bounds <- (idx[, 1] > 0) & (idx[, 1] <= n) & (idx[, 2] > 0) & (idx[, 2] <= p)
      idx <- idx[bounds, ]
      remove <- (sum(rolls_now[idx] == "@") < 4)
      if (remove) {
        any_removed <- TRUE
        rolls_now[i, j] <- "."
        result <- result + 1
      }
    }
  }
}
result
