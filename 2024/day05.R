# Data
data <- readLines("2024/inputs/day05_input.txt")
idx <- which(nchar(data) == 0)
rules <- data[1:(idx - 1)] |>
  strsplit(split = "\\|") |>
  lapply(as.integer)
pages <- data[(idx + 1):length(data)] |>
  strsplit(split = ",") |>
  lapply(as.integer)

# Part 1
# install.packages("igraph")
library("igraph")
edges <- do.call("rbind", rules)
mat <- matrix(0, nrow = max(edges), ncol = max(edges))
mat[edges] <- 1L

find_center <- function(x, mat, correct = TRUE) {
  idx <- length(x) %/% 2 + 1
  adj <- mat[x, x]
  g <- graph_from_adjacency_matrix(adj, mode = "directed")
  g <- set_vertex_attr(g, "names", value = x)
  topo <- topo_sort(g)
  sorted <- V(g)[topo]$names
  if (isTRUE(all.equal(x, sorted))) {
    ifelse(correct, x[idx], 0L)
  } else {
    ifelse(correct, 0L, sorted[idx])
  }
}

result1 <- sum(vapply(pages, find_center, integer(1), mat))
result1

# Part 2
result2 <- sum(vapply(pages, find_center, integer(1), mat, correct = FALSE))
result2

