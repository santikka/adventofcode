# Data
data <- readLines("2024/inputs/day12_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

map <- data |>
  rbind(".", ... = _, ".") |>
  cbind(".", ... = _, ".")

# Part 1
explore_map <- function(map, pos) {
  idx <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  n <- ncol(map)
  planned <- matrix(FALSE, n, n)
  planned[pos[1], pos[2]] <- TRUE
  sides <- idx + rep(pos, each = 4)
  plant <- map[pos[1], pos[2]]
  current_plant <- map[sides] == plant
  to_visit <- sides[current_plant, , drop = FALSE]
  planned[to_visit] <- TRUE
  area <- 1
  perimeter <- sum(!current_plant)
  while (nrow(to_visit) > 0) {
    pos <- to_visit[1, ]
    to_visit <- to_visit[-1, , drop = FALSE]
    sides <- idx + rep(pos, each = 4)
    current_plant <- map[sides] == plant
    not_planned <- !planned[sides]
    area <- area + 1
    perimeter <- perimeter + sum(!current_plant)
    if (any(current_plant & not_planned)) {
      to_visit <- rbind(to_visit, sides[current_plant & not_planned, ])
      planned[to_visit] <- TRUE
    }
  }
  list(
    area = area,
    perimeter = perimeter,
    visited = planned
  )
}

compute_price <- function(map) {
  n <- ncol(map)
  visited <- matrix(FALSE, n - 2, n - 2)
  total_price <- 0
  while (any(!visited)) {
    idx <- which(!visited, arr.ind = TRUE)[1, ]
    out <- explore_map(map, idx + c(1, 1))
    visited <- visited | out$visited[2:(n - 1), 2:(n - 1)]
    total_price <- total_price + out$area * out$perimeter
  }
  total_price
}

result1 <- compute_price(map)
result1

# Part 2
count_sides <- function(shape) {
  idx <- which(shape, arr.ind = TRUE)
  n <- ncol(shape)
  prev_top_edge <- FALSE
  prev_bottom_edge <- FALSE
  prev_left_edge <- FALSE
  prev_right_edge <- FALSE
  sides <- 0
  for (i in 2:(n - 1)) {
    for (j in 2:(n - 1)) {
      top_edge <- !shape[i - 1, j] && shape[i, j]
      if (top_edge && !prev_top_edge) {
        sides <- sides + 1
      }
      bottom_edge <- !shape[i + 1, j] && shape[i, j]
      if (bottom_edge && !prev_bottom_edge) {
        sides <- sides + 1
      }
      prev_top_edge <- top_edge
      prev_bottom_edge <- bottom_edge
    }
  }
  for (j in 2:(n - 1)) {
    for (i in 2:(n - 1)) {
      left_edge <- !shape[i, j - 1] && shape[i, j]
      if (left_edge && !prev_left_edge) {
        sides <- sides + 1
      }
      right_edge <- !shape[i, j + 1] && shape[i, j]
      if (right_edge && !prev_right_edge) {
        sides <- sides + 1
      }
      prev_left_edge <- left_edge
      prev_right_edge <- right_edge
    }
  }
  sides
}

compute_discounted_price <- function(map) {
  n <- ncol(map)
  visited <- matrix(FALSE, n - 2, n - 2)
  total_price <- 0
  while (any(!visited)) {
    idx <- which(!visited, arr.ind = TRUE)[1, ]
    out <- explore_map(map, idx + c(1, 1))
    sides <- count_sides(out$visited)
    visited <- visited | out$visited[2:(n - 1), 2:(n - 1)]
    total_price <- total_price + out$area * sides
  }
  total_price
}

result2 <- compute_discounted_price(map)
result2
