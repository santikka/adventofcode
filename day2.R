# Part 1
data <- readLines("inputs/day2_input.txt")
data <- strsplit(data, ",")[[1L]]
data <- strsplit(data, "-")
ids <- lapply(data, function(x) seq(x[1], x[2]))
twice <- function(x) {
  y <- as.character(x)
  n <- nchar(y)
  half <- n %/% 2L
  heads <- substr(y, 1L, half)
  tails <- substr(y, half + 1L, n)
  sum(x[heads == tails])
}
result <- sum(vapply(ids, twice, double(1)))
result

# Part 2
repeated <- function(x) {
  idx <- grepl("^(\\S+?)\\1+$", x, perl = TRUE)
  sum(x[idx])
}
result <- sum(vapply(ids, repeated, double(1)))
result
