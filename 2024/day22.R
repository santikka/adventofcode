# Data
data <- readLines("2024/inputs/day22_input.txt") |>
  as.integer()

# Part 1
secret_number <- function(nums, iter) {
  limit_first <- 2^18 - 1
  limit_second <- 2^13 - 1
  div <- 2^24 - 1
  for (i in seq_len(iter)) {
    tmp <- bitwAnd(nums, limit_first)
    nums <- bitwAnd(bitwXor(nums, tmp * 64), div)
    nums <- bitwAnd(bitwXor(nums, nums %/% 32), div)
    tmp <- bitwAnd(nums, limit_second)
    nums <- bitwAnd(bitwXor(nums, tmp * 2048), div)
  }
  nums
}

result1 <- sum(secret_number(data, 2000))
result1

# Part 2
get_prices <- function(nums, iter) {
  limit_first <- 2^18 - 1
  limit_second <- 2^13 - 1
  div <- 2^24 - 1
  n <- length(nums)
  prices <- matrix(0, n, iter + 1)
  prices[, 1] <- nums %% 10 
  for (i in seq_len(iter)) {
    tmp <- bitwAnd(nums, limit_first)
    nums <- bitwAnd(bitwXor(nums, tmp * 64), div)
    nums <- bitwAnd(bitwXor(nums, nums %/% 32), div)
    tmp <- bitwAnd(nums, limit_second)
    nums <- bitwAnd(bitwXor(nums, tmp * 2048), div)
    prices[, i + 1] <- nums %% 10
  }
  prices
}

possible_sequences <- function(depth, value, sequence, max_depth, cache) {
  if (depth > max_depth) {
    return(sequence)
  }
  out <- c()
  if (depth == 0) {
    for (i in 0:9) {
      s <- possible_sequences(depth + 1, i, "", max_depth, cache)
      out <- c(out, s)
    }
  } else {
    idx <- paste(depth, value)
    if (!is.null(cache[[idx]])) {
      return(paste0(sequence, cache[[idx]]))
    }
    suffix <- c()
    for (i in (-value):(-value + 9)) {
      s <- possible_sequences(depth + 1, value + i, i, max_depth, cache)
      suffix <- c(suffix, s)
    }
    suffix <- unique(suffix)
    out <- paste0(sequence, suffix)
    cache[[idx]] <- suffix
  }
  unique(out)
}

maximize_bananas <- function(prices) {
  changes <- prices[, -1] - prices[, -ncol(prices)]
  changes <- matrix(as.character(changes), dim(changes))
  n <- nrow(changes)
  m <- ncol(changes)
  cache <- new.env()
  seqs <- possible_sequences(0, 0, "", 4, cache)
  bananas <- matrix(NA, n, length(seqs))
  colnames(bananas) <- seqs
  rownames(bananas) <- 1:n
  idx <- cbind(1:n, "")
  for (i in 1:(m - 3)) {
    idx[, 2] <- paste0(
      changes[, i],
      changes[, i + 1],
      changes[, i + 2],
      changes[, i + 3]
    )
    current <- bananas[idx]
    keep <- which(!is.na(current))
    new_bananas <- prices[, i + 4]
    new_bananas[keep] <- current[keep]
    bananas[idx] <- new_bananas
  }
  bananas[is.na(bananas)] <- 0
  bananas
}

prices <- get_prices(data, 2000)
bananas <- maximize_bananas(prices)
totals <- colSums(bananas)
result2 <- max(totals)
result2
