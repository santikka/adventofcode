# Data
data <- readLines("2023/inputs/day13_input.txt")
idx <- c(0, which(nchar(data) == 0), length(data) + 1)
patterns <- lapply(
  seq_len(length(idx) - 1), 
  function(i) {
    data[(idx[i] + 1):(idx[i + 1] - 1)] |>
      strsplit(split = "") |>
      do.call(what = "rbind")
  }
)

# Part 1
summarize_patterns <- function(patterns) {
  sumr <- 0
  for (i in seq_along(patterns)) {
    pat <- patterns[[i]]
    n <- nrow(pat)
    m <- ncol(pat)
    for (j in 1:(m - 1)) {
      k <- min(j - 1, m - j - 1)
      lhs <- pat[, j:(j - k)]
      rhs <- pat[, (j + 1):(j + 1 + k)]
      sumr <- sumr + j * all(lhs == rhs)
    }
    for (j in 1:(n - 1)) {
      k <- min(j - 1, n - j - 1)
      top <- pat[j:(j - k), ]
      bot <- pat[(j + 1):(j + 1 + k), ]
      sumr <- sumr + j * 100 * all(top == bot)
    }
  }
  sumr
}

result1 <- summarize_patterns(patterns)
result1

# Part 2
fix_smudges <- function(patterns) {
  sumr <- 0
  for (i in seq_along(patterns)) {
    found <- FALSE
    pat <- patterns[[i]]
    n <- nrow(pat)
    m <- ncol(pat)
    for (j in 1:(m - 1)) {
      k <- min(j - 1, m - j - 1)
      lhs <- pat[, (j - k):j]
      rhs <- pat[, (j + 1 + k):(j + 1)]
      if (sum(lhs == rhs) == ((k + 1) * n - 1)) {
        sumr <- sumr + j
        found <- TRUE
        break
      }
    }
    if (!found) {
      for (j in 1:(n - 1)) {
        k <- min(j - 1, n - j - 1)
        top <- pat[(j - k):j, ]
        bot <- pat[(j + 1 + k):(j + 1), ]
        if (sum(top == bot) == ((k + 1) * m - 1)) {
          sumr <- sumr + 100 * j
          break
        }
      }
    }
  }
  sumr
}

result2 <- fix_smudges(patterns)
result2
