# Data
data <- readLines("inputs/day5_input.txt")
empty <- which(!nzchar(data))[1]
fresh <- data[1:(empty - 1)]
fresh <- strsplit(fresh, "-")
fresh <- lapply(fresh, as.numeric)
fresh <- do.call("rbind", fresh)
avail <- as.numeric(data[(empty + 1):length(data)])

# Part 1
result1 <- 0
for (i in seq_along(avail)) {
  result1 <- result1 + 1 * any(fresh[, 1] <= avail[i] & avail[i] <= fresh[, 2])
}
result1

# Part 2
ord <- order(fresh[, 1], fresh[, 2])
fresh <- fresh[ord, ]
result2 <- fresh[1, 2] - fresh[1, 1] + 1
n <- nrow(fresh)
max_idx <- 1
for (i in seq(2, n)) {
  if (fresh[i, 1] <= fresh[max_idx, 2]) {
    diff <- fresh[i, 2] - fresh[max_idx, 2]
    if (diff > 0) {
      max_idx <- i
      result2 <- result2 + diff
    }
  } else {
    max_idx <- i
    result2 <- result2 + fresh[i, 2] - fresh[i, 1] + 1
  }
}
formatC(result2, format = "f", digits = 0)
