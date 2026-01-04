# Data
data <- readLines("2023/inputs/day11_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
total_distance <- function(data, offset) {
  galaxies <- which(data == "#", arr.ind = TRUE)
  rows <- which(apply(data, 1, function(x) all(x == ".")))
  cols <- which(apply(data, 2, function(x) all(x == ".")))
  galaxies[, 1] <- vapply(
    galaxies[, 1],
    function(x) x + (offset - 1) * sum(rows < x),
    numeric(1)
  )
  galaxies[, 2] <- vapply(
    galaxies[, 2],
    function(x) x + (offset - 1) * sum(cols < x),
    numeric(1)
  )
  distances <- dist(galaxies, method = "manhattan")
  sum(distances)
}

result1 <- total_distance(data, offset = 2)
result1

# Part 2
result2 <- total_distance(data, offset = 1e6)
result2
