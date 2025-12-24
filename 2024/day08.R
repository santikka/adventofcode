# Data
data <- readLines("2024/inputs/day08_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
antinodes <- function(map) {
  n <- ncol(map)
  freq <- unique(map[map != "."])
  anti <- matrix(FALSE, n, n)
  for (f in freq) {
    idx <- which(map == f, arr.ind = TRUE)
    m <- nrow(idx)
    if (m > 1) {
      for (i in 1:(m - 1)) {
        for (j in (i + 1):m) {
          d <- idx[i, ] - idx[j, ]
          idx_a <- idx[j, ] + 2 * d
          idx_b <- idx[i, ] - 2 * d
          if (all(idx_a >= 1 & idx_a <= n)) {
            anti[idx_a[1], idx_a[2]] <- TRUE
          }
          if (all(idx_b >= 1 & idx_b <= n)) {
            anti[idx_b[1], idx_b[2]] <- TRUE
          }
        }
      }
    }
  }
  anti
}

result1 <- sum(antinodes(data))
result1

# Part 2
antinodes_interval <- function(map) {
  n <- ncol(map)
  freq <- unique(map[map != "."])
  anti <- matrix(FALSE, n, n)
  for (f in freq) {
    idx <- which(map == f, arr.ind = TRUE)
    m <- nrow(idx)
    if (m > 1) {
      for (i in 1:(m - 1)) {
        for (j in (i + 1):m) {
          anti[idx[i, 1], idx[i, 2]] <- TRUE
          anti[idx[j, 1], idx[j, 2]] <- TRUE
          d <- idx[i, ] - idx[j, ]
          idx_a <- idx[j, ] - d
          idx_b <- idx[i, ] + d
          while (all(idx_a >= 1 & idx_a <= n)) {
            anti[idx_a[1], idx_a[2]] <- TRUE
            idx_a <- idx_a - d
          }
          while (all(idx_b >= 1 & idx_b <= n)) {
            anti[idx_b[1], idx_b[2]] <- TRUE
            idx_b <- idx_b + d
          }
        }
      }
    }
  }
  anti
}

result2 <- sum(antinodes_interval(data))
result2
