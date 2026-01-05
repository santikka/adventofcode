# install.packages("collections")

# Data
data <- readLines("2023/inputs/day17_input.txt") |>
  strsplit(split = "") |>
  lapply(as.integer) |>
  do.call(what = "rbind")

# Part 1
minimize_heat_loss <- function(map, constr) {
  n <- ncol(map)
  dist <- array(Inf, c(n, n, 4, constr[2]))
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  rev_dir <- c(2, 1, 4, 3)
  end <- c(n, n)
  pq <- collections::priority_queue()
  init <- list(pos = c(1, 1), dir = 0, len = 0, loss = 0)
  pq$push(init, priority = 0)
  while (pq$size() > 0) {
    node <- pq$pop()
    pos <- c(node$pos)
    row <- pos[1]
    col <- pos[2]
    dir <- node$dir
    len <- node$len
    loss <- node$loss
    if (all(pos == end)) {
      break
    }
    if (loss > 0 && loss > dist[row, col, dir, len]) {
      next
    }
    for (new_dir in 1:4) {
      if (dir == rev_dir[new_dir]) {
        next
      }
      if (dir == new_dir && len == constr[2]) {
        next
      }
      step <- 1 * (dir == new_dir) + constr[1] * (dir != new_dir)
      new_pos <- pos + step * moves[new_dir, ]
      if (any(new_pos < 1) || any(new_pos > n)) {
        next
      }
      new_row <- new_pos[1]
      new_col <- new_pos[2]
      new_loss <- 0
      if (step == 1) {
        new_loss <- loss + map[new_row, new_col]
        new_len <- (new_dir == dir) * (len + 1) + (new_dir != dir) * 1
      } else {
        new_len <- constr[1]
        if (row == new_row) {
          if (col < new_col) {
            col_seq <- (col + 1):new_col
          } else {
            col_seq <- new_col:(col - 1)
          }
          new_loss <- loss + sum(map[new_row, col_seq])
        }
        if (col == new_col) {
          if (row < new_row) {
            row_seq <- (row + 1):new_row
          } else {
            row_seq <- new_row:(row - 1)
          }
          new_loss <- loss + sum(map[row_seq, new_col])
        }
      }
      if (new_loss < dist[new_row, new_col, new_dir, new_len]) {
        dist[new_row, new_col, new_dir, new_len] <- new_loss
        pq$push(
          list(
            pos = new_pos,
            dir = new_dir,
            len = new_len,
            loss = new_loss
          ), 
          priority = -new_loss
        )
      }
    }
  }
  min(dist[end[1], end[2], , ])
}

result1 <- minimize_heat_loss(data, constr = c(1, 3))
result1

# Part 2
result2 <- minimize_heat_loss(data, constr = c(4, 10))
result2
