# Data
data <- readLines("2024/inputs/day20_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
shortest_path <- function(maze, start, end) {
  nodes <- which(maze == ".", arr.ind = TRUE)
  key <- paste(nodes[, 1], nodes[, 2])
  idx <- match(paste(start[1], start[2]), key)
  maze[start[1], start[2]] <- "#"
  n <- nrow(nodes)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  parent <- matrix(0, nrow = n, ncol = 2)
  queue <- vector(mode = "list", length = n)
  queue[[1]] <- start
  push_idx <- 2
  pop_idx <- 0
  found <- FALSE
  while (pop_idx < push_idx - 1) {
    pop_idx <- pop_idx + 1
    node <- queue[[pop_idx]]
    if (all(node == end)) {
      found <- TRUE
      break
    }
    for (i in 1:4) {
      next_node <- node + moves[i, ]
      if (maze[next_node[1], next_node[2]] == "#") {
        next
      }
      idx <- match(paste(next_node[1], next_node[2]), key)
      maze[next_node[1], next_node[2]] <- "#"
      parent[idx, ] <- node
      queue[[push_idx]] <- next_node
      push_idx <- push_idx + 1
    }
  }
  if (!found) {
    return(NULL)
  }
  node <- end
  path <- c()
  while (node[1] != 0 & node[2] != 0) {
    idx <- match(paste(node[1], node[2]), key)
    node <- parent[idx, ]
    path <- c(idx, path)
  }
  nodes[path, ]
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
