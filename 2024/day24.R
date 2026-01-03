# Data
data <- readLines("2024/inputs/day24_input.txt")
idx <- which(nchar(data) == 0)
inits <- data[1:(idx - 1)] |>
  strsplit(split = ": ")
root <- vapply(inits, "[[", character(1), 1)
values <- vapply(inits, function(x) as.integer(x[[2]]), integer(1)) 
connections <- data[(idx + 1):length(data)] |>
  strsplit(split = " ")
lhs <- vapply(connections, "[[", character(1), 1)
gate <- vapply(connections, "[[", character(1), 2)
rhs <- vapply(connections, "[[", character(1), 3)
out <- vapply(connections, "[[", character(1), 5)
map <- cbind(lhs, rhs, gate)
rownames(map) <- out

# Part 1
simulate_system <- function(nodes, map, cache) {
  if (length(nodes) == 1) {
    if (!is.null(cache[[nodes]])) {
      return(cache[[nodes]])
    }
    pa_lhs <- map[nodes, 1]
    pa_rhs <- map[nodes, 2]
    op_fun <- switch(map[nodes, 3],
      "OR" = bitwOr,
      "AND" = bitwAnd,
      "XOR" = bitwXor
    )
    val_lhs <- simulate_system(pa_lhs, map, cache)
    val_rhs <- simulate_system(pa_rhs, map, cache)
    out <- op_fun(val_lhs, val_rhs)
    cache[[nodes]] <- out
    return(out)
  }
  n <- length(nodes)
  bits <- integer(n)
  for (i in seq_len(n)) {
    bits[i] <- simulate_system(nodes[i], map, cache)
  }
  sum(2^(1:n - 1) * bits)
}

cache <- new.env()
for (i in seq_along(root)) {
  cache[[root[i]]] <- values[i]
}
sink <- sort(out[startsWith(out, "z")])
result1 <- simulate_system(sink, map, cache)
formatC(result1, format = "f", digits = 0)

# Part 2
find_incorrect <- function(root, sink, map) {
  incorrect <- list()
  count <- 0
  idx <- which(map[sink[-length(sink)], 3] !=  "XOR") - 1
  for (i in idx) {
    node <- paste0("z", sprintf("%02d", i))
    incorrect[[node]] <- list(nodes = node, index = i)
  }
  count <- count + length(idx)
  i <- -1
  while (count < 4) {
    i <- i + 1
    if (i %in% idx) {
      next
    }
    node <- paste0("z", sprintf("%02d", i))
    pa <- setdiff(map[node, 1:2], root)
    if (length(pa) > 0 && all(map[pa, 3] != "XOR")) {
      incorrect[[node]] <- list(nodes = pa, index = i)
      count <- count + 1
    }
  }
  incorrect
}

find_swaps <- function(root, sink, map) {
  lhs <- map[, 1]
  rhs <- map[, 2]
  gate <- map[, 3]
  out <- rownames(map)
  incorrect <- find_incorrect(root, sink, map)
  swapped <- c()
  for (i in seq_along(incorrect)) {
    j <- incorrect[[i]]$index
    nodes <- incorrect[[i]]$nodes
    xy <- paste0(c("x", "y"), sprintf("%02d", j))
    ch <- map[lhs %in% xy | rhs %in% xy, , drop = FALSE]
    if (all(nodes %in% sink)) {
      ch <- ch[ch[, 3] == "XOR", , drop = FALSE]
      rn <- rownames(ch)
      ch <- map[lhs %in% rn | rhs %in% rn, , drop = FALSE]
      ch <- ch[ch[, 3] == "XOR", , drop = FALSE]
      swapped <- c(swapped, nodes, rownames(ch))
    } else {
      swapped <- c(swapped, rownames(ch))
    }
  }
  paste0(sort(swapped), collapse = ",")
}

result2 <- find_swaps(root, sink, map)
result2
