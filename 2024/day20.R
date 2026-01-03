# Data
data <- readLines("2024/inputs/day20_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
shortest_path <- function(maze, start, end) {
  maze[start[1], start[2]] <- "#"
  n <- nrow(maze)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  parents <- array(0, dim = c(n, n, 2))
  queue <- vector(mode = "list", length = n * n)
  queue[[1]] <- c(start, 0)
  push_idx <- 2
  pop_idx <- 0
  path_len <- 1
  found <- FALSE
  while (pop_idx < push_idx - 1) {
    pop_idx <- pop_idx + 1
    node <- queue[[pop_idx]]
    if (all(node[1:2] == end)) {
      found <- TRUE
      path_len <- node[3] + 1
      break
    }
    for (i in 1:4) {
      next_node <- node[1:2] + moves[i, ]
      row <- next_node[1]
      col <- next_node[2]
      if (maze[row, col] == "#") {
        next
      }
      maze[row, col] <- "#"
      parents[row, col, ] <- node[1:2]
      queue[[push_idx]] <- c(next_node, node[3] + 1)
      push_idx <- push_idx + 1
    }
  }
  if (!found) {
    return(NULL)
  }
  path <- matrix(0, path_len, 2)
  i <- path_len
  path[i, ] <- end
  row <- end[1]
  col <- end[2]
  while (row != 0 & col != 0) {
    i <- i - 1
    node <- parents[row, col, ]
    path[i, ] <- node
    row <- node[1]
    col <- node[2]
  }
  path
}

best_cheats <- function(path, duration, threshold) {
  n <- nrow(path)
  dist_vec <- dist(path, method = "manhattan")
  best <- 0
  for (i in 1:(n - threshold)) {
    node_idx <- (i + threshold):n
    dist_idx <- n * (i - 1) - i * (i - 1) / 2 + node_idx - i
    d <- dist_vec[dist_idx]
    node_idx <- node_idx[d <= duration]
    d <- d[d <= duration]
    best <- best + sum(node_idx - i - d >= threshold)
  }
  best
}

maze <- data
start <- which(maze == "S", arr.ind = TRUE)
end <- which(maze == "E", arr.ind = TRUE)
maze[rbind(start, end)] <- "."
path <- shortest_path(maze, start, end)
result1 <- best_cheats(path, duration = 2, threshold = 100)
result1

# Part 2
result2 <- best_cheats(path, 20, 100)
result2
