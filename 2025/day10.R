# Data
data <- readLines("2025/inputs/day10_input.txt") |>
  strsplit(split = " ")

lights <- vapply(data, "[[", character(1), 1)
lights <- substring(lights, 2, nchar(lights) - 1) |>
  strsplit(split = "") |>
  lapply(
    function(x) {
      y <- rep(0, length(x))
      y[which(x == "#")] <- 1
      y
    }
  )

buttons <- lapply(data, function(x) x[2:(length(x) - 1)]) |>
  lapply(function(x) gsub("\\(", "c(", x)) |>
  lapply(function(x) lapply(x, function(y) eval(str2lang(y)) + 1))

joltage <- vapply(data, function(x) x[length(x)], character(1)) |>
  gsub(pattern = "\\{", replacement = "c(") |>
  gsub(pattern = "\\}", replacement = ")") |>
  lapply(function(x) eval(str2lang(x)))

# Part 1
result1 <- 0
for (i in seq_along(lights)) {
  l <- lights[[i]]
  n <- length(l)
  b_idx <- rep(0, n)
  b <- lapply(buttons[[i]], function(x) replace(b_idx, x, 1))
  m <- length(b)
  on_off <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), m)))
  best <- m
  for (j in 2:2^m) {
    pressed <- sum(on_off[j, ])
    if (pressed < best) {
      output <- Reduce(bitwXor, b[on_off[j, ]])
      if (all(output == lights[[i]])) {
        best <- pressed
      }
    }
  }
  result1 <- result1 + best
}
result1

# Part 2
# install.packages("Rglpk")
library("Rglpk")
result2 <- 0
for (i in seq_along(joltage)) {
  rhs <- joltage[[i]]
  n <- length(rhs)
  dir <- rep("==", n)
  mat <- vapply(buttons[[i]], function(x) replace(integer(n), x, 1), double(n))
  obj <- rep(1L, ncol(mat))
  sol <- Rglpk_solve_LP(obj, mat, dir, rhs, max = FALSE, types = "I")
  result2 <- result2 + sol$optimum
}
result2
