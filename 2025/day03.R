# Data
data <- readLines("2025/inputs/day03_input.txt") |>
  strsplit(split = "") |>
  lapply(as.integer)

# Part 1
joltage <- vapply(
  data,
  function(z) {
    mat <- outer(z, z, function(x, y) 10 * x + y)
    max(mat[upper.tri(mat)])
  },
  double(1L)
)
result1 <- sum(joltage)
result1

# Part 2
max_joltage <- function(z) {
  n <- length(z)
  out <- 0
  prev <- 0
  for (i in 12:1) {
    idx <- (prev + 1):(n - i + 1)
    max_z <- max(z[idx])
    out <- out + 10^(i - 1) * max_z
    prev <- which(z == max_z)[1]
    z[1:prev] <- 0
  }
  out
}
result2 <- sum(vapply(data, max_joltage, double(1)))
formatC(result2, digits = 0, format = "f")
