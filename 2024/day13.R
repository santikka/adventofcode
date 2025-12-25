# install.packages("Rglpk")
library("Rglpk")

# Data
data <- readLines("2024/inputs/day13_input.txt")
d <- length(data)
button_a <- data[seq(1, d, by = 4)]
button_b <- data[seq(2, d, by = 4)]
prize <- data[seq(3, d, by = 4)]
match_a <- gregexec("[0-9]+", button_a)
xy_a <- regmatches(button_a, match_a) |>
  lapply(as.integer) |>
  do.call(what = "rbind")
match_b <- gregexec("[0-9]+", button_b)
xy_b <- regmatches(button_b, match_b) |>
  lapply(as.integer) |>
  do.call(what = "rbind")
match_prize <- gregexec("[0-9]+", prize)
xy_prize <- regmatches(prize, match_prize) |>
  lapply(as.integer) |>
  do.call(what = "rbind")

# Part 1
count_tokens <- function(a, b, prize, offset = 0) {
  n <- nrow(a)
  dir <- c("==", "==")
  obj <- c(3L, 1L)
  tokens <- 0
  for (i in 1:n) {
    rhs <- prize[i, ] + offset
    mat <- cbind(a[i, ], b[i, ])
    sol <- Rglpk_solve_LP(obj, mat, dir, rhs, max = FALSE, types = "I")
    if (sol$status == 0) {
      tokens <- tokens + sol$optimum
    }
  }
  tokens
}

result1 <- count_tokens(xy_a, xy_b, xy_prize)
result1

# Part 2
result2 <- count_tokens(xy_a, xy_b, xy_prize, offset = 10000000000000)
formatC(result2, format = "f", digits = 0)
