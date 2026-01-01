# Data
data <- readLines("2023/inputs/day09_input.txt") |>
  strsplit(split = " ") |>
  lapply(as.numeric) |>
  do.call(what = "rbind")

# Part 1
compute_predictions <- function(x) {
  n <- nrow(data)
  m <- ncol(data)
  pred <- length(n)
  for (i in seq_len(n)) {
    j <- 1
    d <- diff(x[i, ])
    val <- d[m - j]
    while (any(d != 0)) {
      j <- j + 1
      d <- diff(d)
      val <- val + d[m - j]
    }
    pred[i] <- x[i, m] + val
  }
  sum(pred)
}

result1 <- compute_predictions(data)
result1

# Part 2
result2 <- compute_predictions(data[, ncol(data):1])
result2
