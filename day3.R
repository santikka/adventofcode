# Part 1
data <- readLines("inputs/day3_input.txt")
data <- strsplit(data, "")
data <- lapply(data, as.integer)
joltage <- vapply(
  data,
  function(z) {
    mat <- outer(z, z, function(x, y) 10 * x + y)
    max(mat[upper.tri(mat)])
  },
  double(1L)
)
result <- sum(joltage)
result

# Part 2
max_joltage <- function(z) {
  n <- length(z)
  out <- 0
  prev <- 0
  for (i in 12:1) {
    idx <- (prev + 1):(n - i + 1)
    max_z <- max(z[idx])
    out <- out + 10^(i-1) * max_z
    prev <- which(z == max_z)[1]
    z[1:prev] <- 0
  }
  out
}
result <- sum(vapply(data, max_joltage, double(1)))
formatC(result, digits = 0, format = "f")
