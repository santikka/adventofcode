# Data
data <- readLines("2024/inputs/day07_input.txt") |>
  strsplit(split = ": ")
test_values <- vapply(data, function(x) as.numeric(x[1]), numeric(1))
operands <- lapply(data, function(x) as.numeric(strsplit(x[-1], " ")[[1]]))

# Part 1
add_mult <- function(x, y) {
  c(x + y, x * y)
}

calibrate <- function(expected, values, ops_fun) {
  n <- length(values)
  lhs <- values[1]
  i <- 1
  while (i < n && any(lhs <= expected)) {
    lhs <- ops_fun(lhs, values[i + 1])
    idx <- which(lhs <= expected)
    lhs <- lhs[idx]
    i <- i + 1
  }
  expected * any(lhs == expected)
}

satisfied1 <- mapply(
  calibrate, 
  test_values, 
  operands, 
  MoreArgs = list(ops_fun = add_mult)
)
result1 <- sum(satisfied1)
formatC(result1, format = "f", digits = 0)

# Part 2
`%concat%` <- function(x, y) {
  ny <- nchar(as.character(y))
  y + 10^ny * x
}

add_mult_concat <- function(x, y) {
  c(x + y, x * y, x %concat% y)
}

satisfied2 <- mapply(
  calibrate, 
  test_values[satisfied1 == 0], 
  operands[satisfied1 == 0], 
  MoreArgs = list(ops_fun = add_mult_concat)
)
result2 <- result1 + sum(satisfied2)
formatC(result2, format = "f", digits = 0)
