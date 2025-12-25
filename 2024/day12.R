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
  min_row <- min(idx[, 1]) - 1
  max_row <- max(idx[, 1]) + 1
  min_col <- min(idx[, 2]) - 1
  max_col <- max(idx[, 2]) + 1
  shape <- shape[min_row:max_row, min_col:max_col]
  n <- nrow(shape)
  m <- ncol(shape)
  top <- shape[-1, -c(1, m), drop = FALSE] & 
    !shape[-n, -c(1, m), drop = FALSE]
  bottom <- shape[-n, -c(1, m), drop = FALSE] & 
    !shape[-1, -c(1, m), drop = FALSE]
  left <- shape[-c(1, n), -1, drop = FALSE] & 
    !shape[-c(1, n), -m, drop = FALSE]
  right <- shape[-c(1, n), -m, drop = FALSE] & 
    !shape[-c(1, n), -1, drop = FALSE]
  sides_top <- sum(apply(top, 1, function(x) sum(diff(c(FALSE, x)) == 1)))
  sides_bottom <- sum(apply(bottom, 1, function(x) sum(diff(c(FALSE, x)) == 1)))
  sides_left <- sum(apply(left, 2, function(x) sum(diff(c(FALSE, x)) == 1)))
  sides_right <- sum(apply(right, 2, function(x) sum(diff(c(FALSE, x)) == 1)))
  sides_top + sides_bottom + sides_left + sides_right
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
