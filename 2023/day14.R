# Data
data <- readLines("2023/inputs/day14_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
tilt_north <- function(platform) {
  n <- ncol(platform)
  col_seq <- 1:n
  for (i in 2:n) {
    cols <- col_seq[platform[i, ] == "O"]
    cols_len <- length(cols)
    if (cols_len == 0) {
      next
    }
    above <- (i - 1):1
    row_seq <- 1:(i - 1)
    rows <- integer(cols_len)
    for (j in seq_along(cols)) {
      idx <- row_seq[platform[above, cols[j]] != "."]
      if (length(idx) == 0) {
        rows[j] <- 1
      } else {
        rows[j] <- i - idx[1] + 1
      }
    }
    platform[i, cols] <- "."
    platform[cbind(rows, cols)] <- "O"
  }
  platform
}

total_load <- function(platform) {
  sum(ncol(platform) - which(platform == "O", arr.ind = TRUE)[, 1] + 1)
}

result1 <- total_load(tilt_north(data))
result1

# Part 2
spin_cycle <- function(platform, max_cycles) {
  n <- ncol(platform)
  out <- numeric(max_cycles)
  i <- 0
  while (i < max_cycles) {
    i <- i + 1
    for (j in 1:4) {
      platform <- t(tilt_north(platform))[, n:1]
    }
    out[i] <- total_load(platform)
    if (any(table(out[out > 0]) > 1)) {
      break
    }
  }
  out[out > 0]
}

loads <- spin_cycle(data, 200)
counts <- table(loads)
counts <- sort(counts[counts > 1])[1]
repeating <- as.integer(names(counts))
idx <- which(loads == repeating)
offset <- (1e9 - idx[1]) %% diff(idx)[1]
result2 <- loads[idx[1] + offset]
result2
