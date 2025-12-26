# Data
data <- readLines("2024/inputs/day15_input.txt")
idx <- which(nchar(data) == 0)
warehouse <- data[1:(idx - 1)] |>
  strsplit(split = "") |>
  do.call(what = "rbind")
moves <- data[(idx + 1):length(data)] |>
  paste0(collapse = "") |>
  strsplit(split = "") |>
  unlist()

# Part 1
move_robot <- function(x) {
  if (x[1] == ".") {
    return(c("@", x[-1]))
  }
  i <- 1
  while (x[i] != ".") {
    if (x[i] == "#") {
      return(x)
    }
    i <- i + 1
  }
  c("@", x[1:(i - 1)], x[(i + 1):length(x)])
}

simulate_robot <- function(warehouse, moves) {
  pos <- which(warehouse == "@", arr.ind = TRUE)
  n <- ncol(warehouse)
  for (i in seq_along(moves)) {
     slice <- switch(moves[i],
       "<" = cbind(pos[1], (pos[2] - 1):1),
       ">" = cbind(pos[1], (pos[2] + 1):n),
       "^" = cbind((pos[1] - 1):1, pos[2]),
       "v" = cbind((pos[1] + 1):n, pos[2])
     )
     next_state <- move_robot(warehouse[slice])
     warehouse[slice] <- next_state
     if (next_state[1] == "@") {
       warehouse[pos[1], pos[2]] <- "."
       pos <- slice[1, ]
     }
  }
  gps <- which(warehouse == "O", arr.ind = TRUE)
  gps[, 1] <- (gps[, 1] - 1) * 100
  gps[, 2] <- gps[ ,2] - 1
  sum(gps)
}

result1 <- simulate_robot(warehouse, moves)
result1

# Part 2
make_wide <- function(warehouse) {
  n <- ncol(warehouse)
  wide <- matrix(NA_character_, nrow = n, ncol = n * 2)
  for (i in 1:n) {
    for (j in 1:n) {
      idx <- (2 * j - 1):(2 * j)
      wide[i, idx] <- switch(warehouse[i, j],
        "#" = c("#", "#"),
        "O" = c("[", "]"),
        "@" = c("@", "."),
        "." = c(".", ".")
      )
    }
  }
  wide
}

validate_move <- function(warehouse, visited, pos, dir, prev) {
  object <- warehouse[pos[1], pos[2]]
  if (object == ".") {
    return(list(possible = TRUE, visited = visited))
  }
  if (object == "#") {
    return(list(possible = FALSE, visited = NULL))
  }
  visited[pos[1], pos[2]] <- TRUE
  v_pos <- pos + dir
  v <- validate_move(warehouse, visited, v_pos, dir, object)
  if (!v$possible) {
    return(list(possible = FALSE, visited = NULL))
  }
  possible <- v$possible
  visited <- v$visited
  if (prev != object && prev != ".") {
    h_pos <- pos + c(0, 1 * (object == "[") - 1 * (object == "]"))
    h <- validate_move(warehouse, visited, h_pos, dir, ".")
    possible <- h$possible
    visited <- h$visited
  }
  return(list(possible = possible, visited = visited))
}

simulate_robot_wide <- function(warehouse, moves) {
  wide <- make_wide(warehouse)
  pos <- which(wide == "@", arr.ind = TRUE)
  n <- ncol(wide)
  visited <- matrix(FALSE, nrow = nrow(warehouse), ncol = n)
  for (i in seq_along(moves)) {
    if (moves[i] %in% c("<", ">")) {
      slice <- switch(moves[i],
        "<" = cbind(pos[1], (pos[2] - 1):1),
        ">" = cbind(pos[1], (pos[2] + 1):n)
      )
      next_state <- move_robot(wide[slice])
      wide[slice] <- next_state
      if (next_state[1] == "@") {
        wide[pos[1], pos[2]] <- "."
        pos <- slice[1, ]
      }
    } else {
      dir <- c(1 * (moves[i] == "v") - 1 * (moves[i] == "^"), 0)
      new_pos <- pos + dir
      next_state <- validate_move(wide, visited, new_pos, dir, "@")
      if (next_state$possible) {
        old <- wide[next_state$visited]
        idx <- which(next_state$visited, arr.ind = TRUE)
        wide[idx] <- "."
        idx[, 1] <- idx[, 1] + dir[1]
        idx[, 2] <- idx[, 2] + dir[2]
        wide[idx] <- old
        wide[pos[1], pos[2]] <- "."
        wide[new_pos[1], new_pos[2]] <- "@"
        pos <- new_pos
      }
    }
  }
  gps <- which(wide == "[", arr.ind = TRUE)
  gps[, 1] <- (gps[, 1] - 1) * 100
  gps[, 2] <- gps[, 2] - 1
  sum(gps)
}

result2 <- simulate_robot_wide(warehouse, moves)
result2
