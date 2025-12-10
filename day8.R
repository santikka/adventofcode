# Data
data <- readLines("inputs/day8_input.txt") |>
  strsplit(split = ",") |>
  lapply(FUN = as.numeric) |>
  do.call(what = "rbind")

# Part 1
connections <- 1000
mat <- as.matrix(dist(data))
diag(mat) <- Inf
mat[upper.tri(mat)] <- Inf
idx <- arrayInd(order(mat), .dim = dim(mat))
group <- seq_len(nrow(mat))
new_group <- max(group) + 1
for (i in seq_len(connections)) {
  a <- idx[i, 1]
  b <- idx[i, 2]
  if (group[a] != group[b]) {
    group[group == group[a]] <- new_group
    group[group == group[b]] <- new_group
    new_group <- new_group + 1
  }
}
result1 <- prod(sort(table(group), decreasing = TRUE)[1:3])
result1

# Part 2
group <- seq_len(nrow(mat))
new_group <- max(group) + 1
n_group <- length(unique(group))
i <- 0
while (n_group > 1) {
  i <- i + 1
  a <- idx[i, 1]
  b <- idx[i, 2]
  if (group[a] != group[b]) {
    group[group == group[a]] <- new_group
    group[group == group[b]] <- new_group
    new_group <- new_group + 1
  }
  n_group <- length(unique(group))
}
result2 <- prod(data[idx[i, ], 1])
result2
