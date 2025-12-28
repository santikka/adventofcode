# Data
data <- readLines("2023/inputs/day04_input.txt") |>
  strsplit(split = " +\\| +")

winning <- lapply(data, "[[", 1) |>
  lapply(strsplit, split = ": +") |>
  lapply(getElement, 1) |>
  lapply("[[", 2) |>
  lapply(function(x) as.integer(strsplit(x, " +")[[1]]))
have <- lapply(data, "[[", 2) |>
  lapply(function(x) as.integer(strsplit(x, " +")[[1]]))

# Part 1
result1 <- sum(
  mapply(function(x, y) any(x %in% y) * 2^(sum(x %in% y) - 1), have, winning)
)
result1

# Part 2
count_cards <- function(have, winning) {
  n <- length(have)
  multiplicity <- rep(1, n)
  for (i in 1:n) {
    w <- sum(have[[i]] %in% winning[[i]])
    if (w > 0) {
      new_cards <- rep(0, n)
      new_cards[(i + 1):min(i + w, n)] <- multiplicity[i]
      multiplicity <- multiplicity + new_cards
    }
  }
  sum(multiplicity)
}

result2 <- count_cards(have, winning)
result2
