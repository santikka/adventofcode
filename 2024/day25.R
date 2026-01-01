# Data
data <- readLines("2024/inputs/day25_input.txt")
n <- length(data)
idx <- c(1, which(nchar(data) == 0) + 1, n + 1)
keys <- matrix(0, nrow = 0, ncol = 5)
locks <- matrix(0, nrow = 0, ncol = 5)
for (i in seq_len(length(idx) - 1)) {
  schematic <- strsplit(data[idx[i]:(idx[i + 1] - 1)], "") |>
    do.call(what = "rbind")
  schematic[schematic == "#"] <- 1
  schematic[schematic == "."] <- 0
  schematic <- matrix(as.integer(schematic), dim(schematic))
  heights <- colSums(schematic) - 1
  if (data[idx[i]] == "#####") {
    locks <- rbind(locks, heights)
  } else {
    keys <- rbind(keys, heights)
  }
}

# Part 1
count_pairs <- function(keys, locks) {
  count <- 0
  for (i in seq_len(nrow(keys))) {
    for (j in seq_len(nrow(locks))) {
      count <- count + 1 * all(keys[i, ] + locks[j, ] <= 5)
    }
  }
  count
}

result1 <- count_pairs(keys, locks)
result1
