# Part 1
data <- readLines("inputs/day1_input.txt")
data <- gsub("L", "-", data)
data <- gsub("R", "", data)
moves <- as.integer(data)
result <- sum((50 + cumsum(moves)) %% 100 == 0)
result

# Part 2
position <- c(50, 50 + cumsum(moves))
n <- length(position) - 1
multiples <- seq(
  (min(position) %/% 100) * 100 - 100,
  (max(position) %/% 100) * 100 + 100,
  by = 100
)
crossings <- integer(n)
for (i in seq_len(n)) {
  lo <- min(position[i:(i+1)])
  hi <- max(position[i:(i+1)])
  crossings[i] <- sum(multiples > lo & multiples < hi)
  if (position[i] %in% multiples) {
    crossings[i] <- crossings[i] + 1
  }
}
result <- sum(crossings)
result
