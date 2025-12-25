# Data
data <- readLines("2024/inputs/day10_input.txt") |>
  strsplit(split = "") |>
  lapply(as.integer) |>
  do.call(what = "rbind")
  
# Part 1
trail_score <- function(pos, map, reachable) {
  height <- map[pos[1], pos[2]]
  if (height == 9) {
    reachable[pos[1], pos[2]] <- TRUE
  }
  idx <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  for (i in 1:nrow(idx)) {
    pos_new <- pos + idx[i, ]
    if (all(pos_new >= 1 & pos_new <= ncol(map))) {
      height_new <- map[pos_new[1], pos_new[2]]
      if (height_new == height + 1) {
        reachable <- reachable | trail_score(pos_new, map, reachable)
      }
    }
  }
  return(reachable)
}

reachable <- matrix(FALSE, nrow(data), ncol(data))
trailheads <- which(data == 0, arr.ind = TRUE)
result1 <- sum(apply(trailheads, 1, trail_score, data, reachable))
result1

# Part 2
trail_rating <- function(pos, map) {
  height <- map[pos[1], pos[2]]
  if (height == 9) {
    return(1)
  }
  idx <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  rating <- 0
  for (i in 1:nrow(idx)) {
    pos_new <- pos + idx[i, ]
    if (all(pos_new >= 1 & pos_new <= ncol(map))) {
      height_new <- map[pos_new[1], pos_new[2]]
      if (height_new == height + 1) {
        rating <- rating + trail_rating(pos_new, map)
      }
    }
  }
  return(rating)
}

result2 <- sum(apply(trailheads, 1, trail_rating, data))
result2
