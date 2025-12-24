# Data
data <- readLines("2025/inputs/day11_input.txt") |>
  strsplit(split = ":| ")
devices <- vapply(data, "[[", character(1), 1)
devices <- c(devices, "out")
connections <- lapply(data, function(x) x[3:length(x)])

# Part 1
topological_sort <- function(mat) {
  n <- ncol(mat)
  cs <- colSums(mat)
  topo <- which(cs == 0)
  mat[topo, ] <- 0
  while (length(topo) < n) {
    cs <- colSums(mat)
    topo <- c(topo, setdiff(which(cs == 0), topo))
    mat[topo, ] <- 0
  }
  topo
}

directed_paths <- function(adj, from, to) {
  num_paths <- 0
  n <- nrow(adj)
  children <- which(adj[from, ] == 1)
  if (n %in% children) {
    children <- setdiff(children, n)
    num_paths <- 1
  }
  paths <- cbind(children, 1)
  m <- nrow(paths)
  while (m > 0) {
    node <- paths[1, 1]
    node_paths <- paths[1, 2]
    paths <- paths[-1, , drop = FALSE]
    children <- which(adj[node, ]  == 1)
    if (n %in% children) {
      children <- setdiff(children, n)
      num_paths <- num_paths + node_paths
    }
    merged <- paths[, 1] %in% children
    old <- !merged
    new <- !children %in% paths[, 1]
    merged_paths <- paths[merged, , drop = FALSE]
    merged_paths[, 2] <- merged_paths[, 2] + node_paths
    old_paths <- paths[!merged, , drop = FALSE]
    new_paths <- matrix(ncol = 2, nrow = 0)
    if (any(new)) {
      new_paths <- cbind(children[new], node_paths)
    }
    paths <- rbind(merged_paths, old_paths, new_paths)
    paths <- paths[order(paths[, 1]), , drop = FALSE]
    m <- nrow(paths)
  }
  num_paths
}

n <- length(devices)
adj <- matrix(0, n, n, dimnames = list(devices, devices))
for (i in 1:(n - 1)) {
  adj[i, which(devices %in% connections[[i]])] <- 1
}
topo <- topological_sort(adj)
names(topo) <- devices[topo]
adj <- adj[topo, topo]
result1 <- directed_paths(adj, "you", "out")
result1

# Part 2
first <- min(topo[c("fft", "dac")])
second <- max(topo[c("fft", "dac")])
pos_first <- which(topo == first)
pos_second <- which(topo == second)
first_device <- devices[first]
second_device <- devices[second]

idx_initial <- seq_len(pos_first)
adj_initial <- adj[idx_initial, idx_initial]
paths_initial <- directed_paths(adj_initial, "svr", first_device)

idx_middle <- seq(pos_first, pos_second)
adj_middle <- adj[idx_middle, idx_middle]
paths_middle <- directed_paths(adj_middle, first_device, second_device)

idx_final <- seq(pos_second, n)
adj_final <- adj[idx_final, idx_final]
paths_final <- directed_paths(adj_final, second_device, "out")

result2 <- paths_initial * paths_middle * paths_final
formatC(result2, format = "f", digits = 0)
