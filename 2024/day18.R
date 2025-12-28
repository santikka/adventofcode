# Data
data <- readLines("2024/inputs/day18_input.txt") |>
  strsplit(split = ",") |>
  lapply(as.integer) |>
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
  visited <- logical(n)
  while (node[1] != 0 & node[2] != 0) {
    idx <- match(paste(node[1], node[2]), key)
    visited[idx] <- TRUE
    node <- parent[idx, ]
  }
  nodes[visited, ]
}

first <- 1:1024
n <- 73
maze <- matrix(".", nrow = n, ncol = n)
maze[1, ] <- "#"
maze[n, ] <- "#"
maze[, 1] <- "#"
maze[, n] <- "#"
maze[cbind(data[first, 2] + 2, data[first, 1] + 2)] <- "#"
path <- shortest_path(maze, start = c(2, 2), end = c(n - 1, n - 1))
result1 <- nrow(path) - 1
result1

# Part 2
first_unreachable <- function(maze, path, bytes) {
  i <- 1
  m <- nrow(maze) - 1
  byte <- c(Inf, Inf)
  while (TRUE) {
    byte <- bytes[i, ]
    pos <- rev(byte) + 2
    maze[pos[1], pos[2]] <- "#"
    if (any(path[, 1] == pos[1] & path[, 2] == pos[2])) {
      path <- shortest_path(maze, start = c(2, 2), end = c(m, m))
      if (is.null(path)) {
        break
      }
    }
    i <- i + 1
  }
  paste0(byte[1], ",", byte[2])
}

result2 <- first_unreachable(maze, path, bytes = data[-first, ])
result2
