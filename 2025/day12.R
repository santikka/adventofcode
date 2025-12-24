# Data
data <- readLines("inputs/day12_input.txt")
idx <- max(which(data == ""))
shape_txt <- data[1:idx]
new_shape <- grepl(":", shape_txt, perl = TRUE)
shapes <- lapply(which(new_shape), function(i) shape_txt[(i + 1):(i + 3)]) |>
  lapply(
    function(x) {
      y <- do.call("rbind", strsplit(x, ""))
      idx_mat <- which(y == "#", arr.ind = TRUE)
      dimnames(idx_mat) <- NULL
      # convert index into coordinates with the center as the origin
      idx_mat[, 1] <- 2 - idx_mat[, 1]
      idx_mat[, 2] <- idx_mat[, 2] - 2
      idx_mat <- idx_mat[, c(2, 1)]
      idx_mat[order(idx_mat[, 1], idx_mat[, 2]), ]
    }
  )

plot_shape <- function(mat) {
  row <- 2 - mat[, 2]
  col <- 2 + mat[, 1]
  new <- matrix(".", 3, 3)
  new[cbind(row, col)] <- "#"
  print(new)
}

flip_h <- function(mat) {
  mat[, 2] <- -1 * mat[, 2]
  mat[order(mat[, 1], mat[, 2]), ]
}

flip_v <- function(mat) {
  mat[, 1] <- -1 * mat[, 1]
  mat[order(mat[, 1], mat[, 2]), ]
}

rotate <- function(mat, times = 1) {
  rot <- matrix(c(0, 1, -1, 0), 2, 2)
  out <- mat
  for (i in 1:times) {
    out <- out %*% rot
  }
  out[order(out[, 1], out[, 2]), ]
}

all_shapes <- lapply(
  shapes,
  function(x) {
    vx <- flip_v(x)
    hx <- flip_h(x)
    unique(
      list(
        x,
        rotate(x, 1),
        rotate(x, 2),
        rotate(x, 3),
        vx,
        rotate(vx, 1),
        rotate(vx, 2),
        rotate(vx, 3),
        hx,
        rotate(hx, 1),
        rotate(hx, 2),
        rotate(hx, 3)
      )
    )
  }
)

regions <- data[(idx + 1):length(data)] |>
  strsplit(split = ":")
rectangles <- lapply(regions, "[[", 1) |>
  lapply(function(x) as.integer(strsplit(x, "x")[[1]]))
quantities <- lapply(regions, "[[", 2) |>
  lapply(function(x) as.integer(strsplit(trimws(x), " ")[[1]]))

# Part 1
shape_sizes <- vapply(all_shapes, function(x) nrow(x[[1]]), integer(1))
areas <- vapply(rectangles, prod, double(1))
totals <- vapply(quantities, function(x) sum(x * shape_sizes), integer(1L))
test_fit <- which(totals <= areas)

# Part 2
