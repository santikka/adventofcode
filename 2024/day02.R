# Data
data <- readLines("2024/inputs/day02_input.txt") |>
  strsplit(split = " ") |>
  lapply(as.integer)

# Part 1
is_safe <- function(x) {
  d <- diff(x)
  (all(d < 0) || all(d > 0)) && all(abs(d) <= 3)
}

result1 <- sum(vapply(data, is_safe, logical(1)))
result1

# Part 2
is_safe_dampened <- function(x) {
  for (i in seq_along(x)) {
    y <- x[-i]
    if (is_safe(y)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

result2 <- sum(vapply(data, is_safe_dampened, logical(1)))
result2
