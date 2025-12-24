# Data
data <- readLines("inputs/day01_input.txt") |>
  gsub(pattern = "L", replacement = "-") |>
  gsub(pattern = "R", replacement = "")

# Part 1
moves <- as.integer(data)
result1 <- sum((50 + cumsum(moves)) %% 100 == 0)
result1

# Part 2
position <- c(50, 50 + cumsum(moves))
n <- length(position) - 1
crossings <- integer(n)
multiples <- seq(
  (min(position) %/% 100) * 100 - 100,
  (max(position) %/% 100) * 100 + 100,
  by = 100
)
for (i in seq_len(n)) {
  lo <- min(position[i:(i+1)])
  hi <- max(position[i:(i+1)])
  crossings[i] <- sum(multiples > lo & multiples < hi)
  if (position[i] %in% multiples) {
    crossings[i] <- crossings[i] + 1
  }
}
result2 <- sum(crossings)
result2
