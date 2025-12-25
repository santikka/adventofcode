# Data
data <- readLines("2024/inputs/day09_input.txt") |>
  strsplit(split = "")
data <- data[[1]]

# Part 1
to_blocks <- function(x) {
  n <- length(x)
  m <- length(x) %/% 2L + 1L
  file_idx <- 0L
  out <- character()
  for (i in seq_len(m)) {
    file <- rep(file_idx, x[2 * i - 1])
    if (2 * i <= n) {
      free <- rep(".", x[2 * i])
      out <- c(out, file, free)
    } else {
      out <- c(out, file)
    }
    file_idx <- file_idx + 1L
  }
  out
}

compact_blocks <- function(blocks) {
  free <- blocks == "."
  free_idx <- which(free)
  file_idx <- which(!free)
  n <- length(file_idx)
  k <- 1L
  first <- free_idx[k]
  last <- file_idx[n]
  while (first < last) {
    blocks[first] <- blocks[last]
    blocks[last] <- "."
    n <- n - 1
    k <- k + 1
    first <- free_idx[k]
    last <- file_idx[n]
  }
  blocks
}

checksum <- function(blocks) {
  blocks[blocks == "."] <- "0"
  nums <- as.integer(blocks)
  sum(nums * seq(0, length(nums) - 1))
}

blocks <- to_blocks(data)
compacted1 <- compact_blocks(blocks)
result1 <- checksum(compacted1)
formatC(result1, format = "f", digits = 0)

# Part 2
compact_files <- function(blocks, files, free) {
  n_files <- length(files)
  files_idx <- seq(0, n_files - 1)
  free_pos <- (1 + cumsum(files) + c(0, cumsum(free)))[-n_files]
  files_pos <- 1 + c(0, cumsum(files[-n_files]) + cumsum(free))
  for (i in seq(n_files, 2)) {
    fit_idx <- which(free[1:(i - 1)] >= files[i])
    if (length(fit_idx) > 0) {
      idx <- fit_idx[1]
      old_idx <- seq(files_pos[i], files_pos[i] + files[i] - 1)
      new_idx <- seq(free_pos[idx], free_pos[idx] + files[i] - 1)
      blocks[new_idx] <- files_idx[i]
      blocks[old_idx] <- "."
      free_pos[idx] <- free_pos[idx] + files[i]
      free[idx] <- free[idx] - files[i]
    }
  }
  blocks
}

files <- as.integer(data[c(TRUE, FALSE)])
free <- as.integer(data[c(FALSE, TRUE)])
compacted2 <- compact_files(blocks, files, free)
result2 <- checksum(compacted2)
formatC(result2, format = "f", digits = 0)
