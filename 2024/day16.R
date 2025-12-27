# install.packages("collections")

# Data
data <- readLines("2024/inputs/day16_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
rev_dir <- function(i) {
  1 * (i == 2) + 2 * (i == 1) + 3 * (i == 4) + 4 * (i == 3)
}

traverse_maze <- function(maze, start, end, init) {
  dist <- which(maze == ".", arr.ind = TRUE)
  dist <- cbind(dist, matrix(Inf, nrow = nrow(dist), ncol = 4))
  dist <- rbind(
    dist,
    c(end, rep(Inf, 4)),
    c(start, rep(0, 4))
  )
  dist <- dist[order(dist[, 1], dist[, 2]), ]
  key <- paste(dist[, 1], dist[, 2])
  n <- nrow(dist)
  m <- length(init)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  pq <- collections::priority_queue()
  pq$push(init, priority = 0)
  while (pq$size() > 0) {
    node <- pq$pop()
    pos <- c(node$pos)
    score <- node$score
    dir <- node$dir
    if (all(pos == end)) {
      break
    }
    idx <- match(paste(pos[1], pos[2]), key)
    if (score > dist[idx, 2 + dir]) {
      next
    }
    for (j in 1:4) {
      if (dir == rev_dir(j)) {
        next
      }
      new_dir <- moves[j, ]
      new_pos <- pos + new_dir
      if (maze[new_pos[1], new_pos[2]] == "#") {
        next
      }
      new_score <- score + 1 + 1000 * (dir != j)
      idx <- match(paste(new_pos[1], new_pos[2]), key)
      if (new_score < dist[idx, 2 + j]) {
        dist[idx, 2 + j] <- new_score
        pq$push(
          list(score = new_score, pos = new_pos, dir = j), 
          priority = -new_score
        )
      }
    }
  }
  dist
}

start <- which(data == "S", arr.ind = TRUE)
end <- which(data == "E", arr.ind = TRUE)
init <- list(score = 0, pos = start, dir = 4)
dist <- traverse_maze(data, start = start, end = end, init = init)
idx <- which(dist[, 1] == end[1] & dist[, 2] == end[2])
result1 <- min(dist[idx, 3:6])
result1

# Part 2
optimal_tiles <- function(dist, dist_rev, optimal) {
  n <- nrow(dist)
  tiles <- logical(n)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  for (i in seq_len(n)) {
    for (j in 1:4) {
      for (k in 1:4) {
        if (length(idx) > 0) {
          diff_edge <- (j %in% c(1, 2) && k %in% c(3, 4)) ||
            (j %in% c(3, 4) && k %in% c(1, 2))
          turn_offset <- 1000 * diff_edge
          if (dist[i, 2 + j] + dist_rev[i, 2 + k] + turn_offset == optimal) {
            tiles[i] <- TRUE
          }
        }
      }
    
    }
  }
  tiles
}

start2 <- end
end2 <- start
start_dir <- rev_dir(which.min(dist[idx, 3:6]))
init2 <- list(score = 0, pos = start2, dir = start_dir)
dist_rev <- traverse_maze(data, start = start2, end = end2, init = init2)
result2 <- sum(optimal_tiles(dist, dist_rev, result1))
result2
