# Data
data <- readLines("2025/inputs/day12_input.txt")
idx <- max(which(data == ""))
shape_txt <- data[1:idx]
new_shape <- grepl(":", shape_txt, perl = TRUE)
shapes <- lapply(which(new_shape), function(i) shape_txt[(i + 1):(i + 3)]) |>
  lapply(
    function(x) {
      y <- do.call("rbind", strsplit(x, ""))
      idx_mat <- which(y == "#", arr.ind = TRUE)
      dimnames(idx_mat) <- NULL
      idx_mat[, 1] <- 2 - idx_mat[, 1]
      idx_mat[, 2] <- idx_mat[, 2] - 2
      idx_mat <- idx_mat[, c(2, 1)]
      idx_mat[order(idx_mat[, 1], idx_mat[, 2]), ]
    }
  )

regions <- data[(idx + 1):length(data)] |>
  strsplit(split = ":")
rectangles <- lapply(regions, "[[", 1) |>
  lapply(function(x) as.integer(strsplit(x, "x")[[1]]))
quantities <- lapply(regions, "[[", 2) |>
  lapply(function(x) as.integer(strsplit(trimws(x), " ")[[1]]))

# Part 1
shape_sizes <- vapply(shapes, nrow, integer(1))
areas <- vapply(rectangles, prod, double(1))
totals <- vapply(quantities, function(x) sum(x * shape_sizes), integer(1))
viable <- which(totals <= areas)
naive_totals <- vapply(quantities, function(x) sum(x * 9L), integer(1))
result <- sum(naive_totals[viable] <= areas[viable])
result
