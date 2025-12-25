# Data
data <- readLines("2024/inputs/day11_input.txt") |>
  strsplit(split = " ")
stones <- as.integer(data[[1]])

# Part 1
next_stone <- function(x) {
  if (x == 0) {
    return(1)
  }
  digits <- floor(log(x, base = 10)) + 1
  if (digits %% 2 == 0) {
    div <- 10^(digits %/% 2)
    rhs <- x %% div
    lhs <- x %/% div
    return(c(lhs, rhs))
  }
  return(x * 2024)
}

apply_rules <- function(values, n) {
  count <- rep(1, length(values))
  for (i in 1:n) {
    new_values <- double()
    new_count <- double()
    for (j in seq_along(values)) {
      new <- next_stone(values[j])
      for (k in seq_along(new)) {
        if (new[k] %in% new_values) {
          idx <- which(new_values == new[k])
          new_count[idx] <- new_count[idx] + count[j]
        } else {
          new_values <- c(new_values, new[k])
          new_count <- c(new_count, count[j])
        }
      }
    }
    values <- new_values
    count <- new_count
  }
  sum(count)
}

result1 <- apply_rules(stones, n = 25)
formatC(result1, format = "f", digits = 0)

# Part 2
result2 <- apply_rules(stones, n = 75)
formatC(result2, format = "f", digits = 0)
