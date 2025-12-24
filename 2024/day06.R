# Data
data <- readLines("2024/inputs/day06_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
n <- ncol(data)
visited <- matrix(FALSE, n, n)
init <- which(data == "^", arr.ind = TRUE)
idx <- init
move <- matrix(c(-1L, 0L), 1, 2)
rotation <- matrix(c(0L, 1L, -1L, 0L), 2, 2)
while (all(idx >= 1L & idx <= n)) {
  visited[idx] <- TRUE
  idx_next <- idx + move
  next_valid <- all(idx_next >= 1L & idx_next <= n)
  while (next_valid && data[idx_next] == "#") {
    move <- move %*% rotation
    idx_next <- idx + move
    next_valid <- all(idx_next >= 1L & idx_next <= n)
  }
  idx <- idx + move
}
result1 <- sum(visited)
result1

# Part 2
is_looping <- function(data, position) {
  data[position[1], position[2]] <- "O"
  n <- ncol(data)
  visited <- array(0L, dim = c(n, n, 2))
  idx <- which(data == "^", arr.ind = TRUE)
  move <- matrix(c(-1L, 0L), 1, 2)
  rotation <- matrix(c(0L, 1L, -1L, 0L), 2, 2)
  while (all(idx >= 1L & idx <= n)) {
    vis_dir <- visited[idx[1], idx[2], ] 
    if (all(vis_dir == move)) {
      return(TRUE)
    }
    visited[idx[1], idx[2], ] <- move
    idx_next <- idx + move
    next_valid <- all(idx_next >= 1L & idx_next <= n)
    while (next_valid && data[idx_next] %in% c("#", "O")) {
      move <- move %*% rotation
      idx_next <- idx + move
      next_valid <- all(idx_next >= 1L & idx_next <= n)
    }
    idx <- idx + move
  }
  return(FALSE)
}

visited[init] <- FALSE # initial position not allowed
visited_pos <- which(visited, arr.ind = TRUE)
looping_pos <- vapply(
  seq_len(nrow(visited_pos)), 
  function(i) is_looping(data, visited_pos[i, ]),
  logical(1)
)
result2 <- sum(looping_pos)
result2
