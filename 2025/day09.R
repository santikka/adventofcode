# Data
data <- readLines("inputs/day09_input.txt") |>
  strsplit(split = ",") |>
  lapply(FUN = as.numeric) |>
  do.call(what = "rbind")
x <- data[, 1]
y <- data[, 2]
edges <- data.frame(x1 = x, x2 = c(x[-1], x[1]), y1 = y, y2 = c(y[-1], y[1]))
edges <- apply(
  edges,
  1,
  function(edge) {
    c(
      x1 = min(edge[1:2]),
      x2 = max(edge[1:2]),
      y1 = min(edge[3:4]),
      y2 = max(edge[3:4])
    )
  },
  simplify = FALSE
) |>
  do.call(what = "rbind")

# Part 1
n <- nrow(data)
max_rect <- 0
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    width <- abs(x[i] - x[j]) + 1
    height <- abs(y[i] - y[j]) + 1
    rect <- width * height
    if (rect > max_rect) {
      max_rect <- rect
    }
  }
}
result1 <- max_rect
result1

# Part 2
test_intersect <- function(x1, x2, y1, y2, edges) {
  v <- (edges[, "y1"] != edges[,"y2"]) &
    (x1 < edges[, "x1"] & edges[, "x2"] < x2)
  h <- (edges[, "x1"] != edges[,"x2"]) &
    (y1 < edges[, "y1"] & edges[, "y2"] < y2)
  v_edges <- edges[v, , drop = FALSE]
  h_edges <- edges[h, , drop = FALSE]
  any(h_edges[, "x1"] <= x1 & h_edges[, "x2"] > x1) || # left
    any(h_edges[, "x2"] >= x2 & h_edges[, "x1"] < x2) || # right
    any(v_edges[, "y1"] <= y1 & v_edges[, "y2"] > y1) || # bottom
    any(v_edges[, "y2"] >= y2 & v_edges[, "y1"] < y2) # top
}

max_rect <- 0
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    x_min <- min(x[i], x[j])
    x_max <- max(x[i], x[j])
    y_min <- min(y[i], y[j])
    y_max <- max(y[i], y[j])
    width <- x_max - x_min + 1
    height <- y_max - y_min + 1
    rect <- width * height
    if (rect <= max_rect) {
      next
    }
    if (!test_intersect(x_min, x_max, y_min, y_max, edges)) {
      max_rect <- rect
    }
  }
}
result2 <- max_rect
result2
