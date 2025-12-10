# Data
data <- readLines("inputs/day7_input.txt") |>
  strsplit(split = "") |>
  lapply(function(x) which(x != "."))
diagram <- data[lengths(data) > 0][-1]

# Part 1
n <- length(diagram)
visited <- matrix(FALSE, nrow = n, ncol = max(unlist(diagram)) + 1)
traverse <- function(level, position) {
  if (level == (n + 1) || visited[level, position]) {
    return(0)
  }
  visited[level, position] <<- TRUE
  if (position %in% diagram[[level]]) {
    left <- traverse(level + 1, position - 1)
    right <- traverse(level + 1, position + 1)
    return(1 + left + right)
  }
  return(traverse(level + 1, position))
}
result1 <- traverse(1, diagram[[1]])
result1

# Part 2
timelines <- matrix(c(diagram[[1]], 1), 1, 2)
i <- 1
while(i <= n) {
  split <- timelines[, 1] %in% diagram[[i]]
  whole <- !timelines[, 1] %in% diagram[[i]]
  left <- timelines[split, 1] - 1
  right <- timelines[split, 1] + 1
  center <- timelines[whole, 1]
  new <- cbind(sort(unique(c(left, right, center))), 0)
  l_idx <- new[, 1] %in% left
  r_idx <- new[, 1] %in% right
  c_idx <- new[, 1] %in% center
  new[l_idx, 2] <- timelines[split, 2]
  new[r_idx, 2] <- new[r_idx, 2] + timelines[split, 2]
  new[c_idx, 2] <- new[c_idx, 2] + timelines[whole, 2]
  timelines <- new[order(new[, 1]), ]
  i <- i + 1
}
result2 <- sum(timelines[, 2])
formatC(result2, format = "f", digits = 0)
