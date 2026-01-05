# Data
data <- readLines("2023/inputs/day16_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
energize_grid <- function(pos, dir, cache) {
  reflect_forward <- c(4, 3, 2, 1)
  reflect_backward <- c(3, 4, 1, 2)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  while (all(pos >= 1) && all(pos <= cache$n)) {
    row <- pos[1]
    col <- pos[2]
    sym <- cache$grid[row, col]
    if (sym == "|") {
      if (dir == 3 || dir == 4) {
        if (cache$energized[row, col]) {
          return(invisible(NULL))
        }
        cache$energized[row, col] <- TRUE
        energize_grid(pos + moves[1, ], 1, cache)
        energize_grid(pos + moves[2, ], 2, cache)
        return(invisible(NULL))
      }
    } else if (sym == "-") {
      if (dir == 1 || dir == 2) {
        if (cache$energized[row, col]) {
          return(invisible(NULL))
        }
        cache$energized[row, col] <- TRUE
        energize_grid(pos + moves[3, ], 3, cache)
        energize_grid(pos + moves[4, ], 4, cache)
        return(invisible(NULL))
      }
    } else if (sym == "/") {
      dir <- reflect_forward[dir]
    } else if (sym == "\\") {
      dir <- reflect_backward[dir]
    }
    cache$energized[row, col] <- TRUE
    pos <- pos + moves[dir, ]
  }
  invisible(NULL)
}

total_energized <- function(pos, dir, grid) {
  n <- ncol(grid)
  cache <- new.env()
  cache$n <- n
  cache$grid <- grid
  cache$energized <- matrix(FALSE, n, n)
  energize_grid(pos, dir, cache)
  sum(cache$energized)
}

result1 <- total_energized(c(1, 1), 4, data)
result1

# Part 2
maximize_energized <- function(grid) {
  n <- ncol(grid)
  cache <- new.env()
  cache$n <- n
  cache$grid <- grid
  cache$energized <- matrix(FALSE, n, n)
  energized <- 0
  for (k in 1:n) {
    cache$energized[] <- FALSE
    energize_grid(c(n, k), 1, cache)
    up <- sum(cache$energized)
    cache$energized[] <- FALSE
    energize_grid(c(1, k), 2, cache)
    down <- sum(cache$energized)
    cache$energized[] <- FALSE
    energize_grid(c(k, n), 3, cache)
    left <- sum(cache$energized)
    cache$energized[] <- FALSE
    energize_grid(c(k, 1), 4, cache)
    right <- sum(cache$energized)
    energized <- max(energized, up, down, left, right)
  }
  energized
}

result2 <- maximize_energized(data)
result2
