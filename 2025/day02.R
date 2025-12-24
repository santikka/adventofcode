# Data
data <- readLines("2025/inputs/day02_input.txt")
data <- strsplit(data, ",")[[1L]] |>
  strsplit(split = "-")
ids <- lapply(data, function(x) seq(x[1], x[2]))

# Part 1
twice <- function(x) {
  y <- as.character(x)
  n <- nchar(y)
  half <- n %/% 2L
  heads <- substr(y, 1L, half)
  tails <- substr(y, half + 1L, n)
  sum(x[heads == tails])
}
result1 <- sum(vapply(ids, twice, double(1)))
result1

# Part 2
repeated <- function(x) {
  idx <- grepl("^(\\S+?)\\1+$", x, perl = TRUE)
  sum(x[idx])
}
result2 <- sum(vapply(ids, repeated, double(1)))
result2
