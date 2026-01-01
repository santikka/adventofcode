# Data
data <- readLines("2023/inputs/day08_input.txt")
moves <- c("L" = 1, "R" = 2)
moves <- moves[strsplit(data[1], "")[[1]]]
maps <- data[3:length(data)] |>
  strsplit(split = " = ")
nodes <- vapply(maps, "[[", character(1), 1)
conn <- vapply(
  maps, 
  function(x) substring(x[2], 2, nchar(x[2]) - 1),
  character(1)
) |>
  lapply(function(x) strsplit(x, split = ", ")[[1]])
lhs <- vapply(conn, "[[", character(1), 1)
rhs <- vapply(conn, "[[", character(1), 2)
g <- cbind(lhs, rhs)
rownames(g) <- nodes

# Part 1
navigate_graph <- function(graph, node, end, moves) {
  n <- length(moves)
  idx <- 0
  steps <- 0
  while (!node %in% end) {
    node <- graph[node, moves[idx + 1]]
    steps <- steps + 1
    idx <- (idx + 1) %% n
  }
  steps
}

result1 <- navigate_graph(g, "AAA", "ZZZ", moves)
result1

# Part 2
a_nodes <- nodes[endsWith(nodes, "A")]
z_nodes <- nodes[endsWith(nodes, "Z")]
an <- length(a_nodes)
steps <- numeric(an)
for (i in seq_len(an)) {
  steps[i] <- navigate_graph(g, a_nodes[i], z_nodes, moves)
}
m <- length(moves)
result2 <- prod(steps / m) * m
formatC(result2, format = "f", digits = 0)
