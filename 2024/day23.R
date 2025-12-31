# install.packages("igraph")
library("igraph")

# Data
data <- readLines("2024/inputs/day23_input.txt") |>
  strsplit(split = "-") |>
  do.call(what = "rbind")
g <- graph_from_edgelist(data, directed = FALSE)

# Part 1
count_cliques <- function(g) {
  t_nodes <- V(g)[startsWith(name, "t")] |>
    as.integer()
  t_neigh <- lapply(t_nodes, neighbors, graph = g) |>
    unlist() |>
    as.integer() |>
    unique()
  g_t <- subgraph(g, union(t_nodes, t_neigh))
  t_nodes <- V(g_t)[startsWith(name, "t")]
  cliq <- cliques(g_t, min = 3, max = 3)
  vapply(cliq, function(x) any(as.integer(x) %in% t_nodes), logical(1)) |>
    sum()
}

result1 <- count_cliques(g)
result1

# Part 2
largest <- largest_cliques(g)[[1]]
nm <- V(g)$name[largest]
result2 <- paste0(sort(nm), collapse = ",")
result2
