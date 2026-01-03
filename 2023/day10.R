# Data
data <- readLines("2023/inputs/day10_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
create_valid_symbols <- function() {
  valid_moves <- array("", dim = c(4, 6, 4))
  dimnames(valid_moves)[[2]] <- c("F", "J", "L", "7", "-", "|")
  valid_moves[1, "J", ] <- c("|", "F", "7", "S")
  valid_moves[1, "L", ] <- c("|", "F", "7", "S")
  valid_moves[1, "|", ] <- c("|", "F", "7", "S")
  valid_moves[2, "F", ] <- c("|", "J", "L", "S")
  valid_moves[2, "7", ] <- c("|", "J", "L", "S")
  valid_moves[2, "|", ] <- c("|", "J", "L", "S")
  valid_moves[3, "7", ] <- c("-", "F", "L", "S")
  valid_moves[3, "J", ] <- c("-", "F", "L", "S")
  valid_moves[3, "-", ] <- c("-", "F", "L", "S")
  valid_moves[4, "F", ] <- c("-", "J", "7", "S")
  valid_moves[4, "L", ] <- c("-", "J", "7", "S")
  valid_moves[4, "-", ] <- c("-", "J", "7", "S")
  valid_moves
}

find_farthest <- function(diagram) {
  diagram <- diagram |>
    rbind(".", ... = _, ".") |>
    cbind(".", ... = _, ".")
  start <- which(diagram == "S", arr.ind = TRUE)
  valid_syms <- create_valid_symbols()
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  rev_dir <- c(2, 1, 4, 3)
  initial <- c()
  syms <- c("|", "|", "-", "-")
  for (dir in 1:4) {
    pos <- start + moves[dir, ]
    if (diagram[pos] %in% valid_syms[dir, syms[dir], ]) {
      initial <- c(initial, dir)
    }
  }
  nodes <- rbind(start + moves[initial[1], ], start + moves[initial[2], ])
  syms <- c(diagram[nodes])
  dirs <- initial
  max_dist <- 1
  while (!all(nodes[1, ] == nodes[2, ])) {
    max_dist <- max_dist + 1
    for (i in 1:2) {
      for (j in 1:4) {
        if (j == rev_dir[dirs[i]]) {
          next
        }
        next_node <- nodes[i, ] + moves[j, ]
        next_sym <- diagram[next_node[1], next_node[2]]
        if (next_sym == ".") {
          next
        }
        if (next_sym %in% valid_syms[j, syms[i], ]) {
          nodes[i, ] <- next_node
          syms[i] <- next_sym
          dirs[i] <- j
          break
        }
      }
    }
  }
  max_dist
}

result1 <- find_farthest(data)
result1

# Part 2
find_loop <- function(diagram) {
  diagram <- diagram |>
    rbind(".", ... = _, ".") |>
    cbind(".", ... = _, ".")
  start <- which(diagram == "S", arr.ind = TRUE)
  nodes <- which(diagram != ".", arr.ind = TRUE)
  parents <- matrix(0, nrow = nrow(nodes), ncol = 3)
  pa <- 1
  valid_syms <- create_valid_symbols()
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  rev_dir <- c(2, 1, 4, 3)
  syms <- c("|", "|", "-", "-")
  dir <- 1
  node <- start + moves[dir, ]
  while (!diagram[node] %in% valid_syms[dir, syms[dir], ]) {
    dir <- dir + 1
    node <- start + moves[dir, ]
  }
  sym <- diagram[node]
  parents[pa, ] <- c(start, dir)
  pa <- 2
  loop_len <- 1
  while (!all(node == start)) {
    loop_len <- loop_len + 1
    for (i in 1:4) {
      if (i == rev_dir[dir]) {
        next
      }
      next_node <- node + moves[i, ]
      next_sym <- diagram[next_node[1], next_node[2]]
      if (next_sym %in% valid_syms[i, sym, ]) {
        parents[pa, ] <- c(node, i)
        node <- next_node
        sym <- next_sym
        dir <- i
        pa <- pa + 1
        break
      }
    }
  }
  loop <- parents[seq_len(loop_len), ]
  loop[, 1:2] <- loop[, 1:2] - 1
  loop
}

count_enclosed <- function(diagram) {
  loop <- find_loop(diagram)
  expand <- matrix(".", 2 * nrow(diagram), 2 * ncol(diagram))
  expand[2 * loop[, 1:2]] <- diagram[loop[, 1:2]]
  loop[, 1:2] <- 2 * loop[, 1:2]
  diagram <- expand |>
    rbind(".") |>
    cbind(".")
  n <- nrow(diagram)
  m <- ncol(diagram)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  syms <- c("|", "|", "-", "-")
  for (i in seq_len(nrow(loop))) {
    dir <- loop[i, 3]
    node <- loop[i, 1:2] + moves[dir, ]
    diagram[node[1], node[2]] <- syms[dir]
  }
  queue <- vector(mode = "list", length = sum(diagram == "."))
  queue[[1]] <- c(1, 1)
  push_idx <- 2
  pop_idx <- 0
  while (pop_idx < push_idx - 1) {
    pop_idx <- pop_idx + 1
    node <- queue[[pop_idx]]
    for (i in 1:4) {
      next_node <- node + moves[i, ]
      if (any(next_node < 1) | next_node[1] > n | next_node[2] > m) {
        next
      }
      if (diagram[next_node[1], next_node[2]] != ".") {
        next
      }
      diagram[next_node[1], next_node[2]] <- "O"
      queue[[push_idx]] <- next_node
      push_idx <- push_idx + 1
    }
  }
  empty <- which(diagram == ".", arr.ind = TRUE)
  sum(empty[, 1] %% 2 == 0 & empty[, 2] %% 2 == 0)
}

result2 <- count_enclosed(data)
result2
