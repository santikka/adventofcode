# Data
data <- readLines("2024/inputs/day21_input.txt")

keypad_num <- matrix(
  c(
    1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4,
    1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3
  ),
  ncol = 2
)
nm_num <- c("7", "4", "1", "8", "5", "2", "0", "9", "6", "3", "A")
rownames(keypad_num) <- nm_num
cost_num <- dist(keypad_num, method = "manhattan")

keypad_dir <- matrix(
  c(
    2, 1, 2, 1, 2,
    1, 2, 2, 3, 3
  ),
  ncol = 2
)
nm_dir <- c("<", "^", "v", "A", ">") 
rownames(keypad_dir) <- nm_dir
cost_dir <- dist(keypad_dir, method = "manhattan")

banned_num <- c(
  `0` = "<",
  `1` = "v",
  `2` = "x",
  `3` = "x",
  `4` = "vv",
  `5` = "x",
  `6` = "x",
  `7` = "vvv",
  `8` = "x",
  `9` = "x",
  `A` = "<<"
)

banned_dir <- c(
  `<` = "^",
  `^` = "<",
  `A` = "<<",
  `v` = "x",
  `>` = "x"
)

# Part 1
num_changes <- function(x) {
  x_vec <- strsplit(x, "")[[1]]
  n <- length(x_vec)
  if (n == 1) {
    return(0)
  }
  sum(x_vec[1:(n - 1)] != x_vec[2:n])
}

get_permutations <- function(x) {
  n <- length(x)
  if (n == 1) {
    return(as.character(x))
  }
  out <- vector(mode = "list", length = n)
  for (i in 1:n) {
    out[[i]] <- paste0(x[i], get_permutations(x[-i]))
  }
  unlist(out)
}

total_cost <- function(syms_str, cost) {
  syms <- strsplit(syms_str, "")[[1]]
  mat <- as.matrix(cost)
  pos <- "A"
  total <- 0
  for (sym in syms) {
    total <- total + mat[pos, sym]
    pos <- sym
  }
  total <- total + mat[pos, "A"]
  total
}

optimal_sequences <- function(keypad, banned, cost) {
  n <- nrow(keypad)
  out <- matrix("", n, n)
  nm <- rownames(keypad)
  dimnames(out) <- list(nm, nm)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        next
      }
      syms <- c()
      h <- keypad[j, 2] - keypad[i, 2]
      v <- keypad[j, 1] - keypad[i, 1]
      h_sym <- ifelse(h > 0, ">", "<")
      v_sym <- ifelse(v < 0, "^", "v")
      syms <- c(syms, rep(h_sym, abs(h)), rep(v_sym, abs(v)))
      perm <- unique(get_permutations(syms))
      min_change <- Inf
      min_cost <- Inf
      min_idx <- 0
      for (k in seq_along(perm)) {
        if (startsWith(perm[k], banned[nm[i]])) {
          next
        }
        prefix <- substring(perm[k], 1, 1)
        # + 0.5 as a tie breaker for > vs ^
        total <- total_cost(perm[k], cost) + 0.5 * (prefix == ">")
        if (total < min_cost) {
          min_change <- num_changes(perm[k])
          min_cost <- total
          min_idx <- k
        } else if (total == min_cost) {
          change <- num_changes(perm[k])
          if (change < min_change) {
            min_change <- change
            min_idx <- k
          }
        }
      }
      out[i, j] <- perm[min_idx]
    }
  }
  out
}

count_pairs <- function(x, count_in, u_pairs) {
  n <- length(x)
  len <- nchar(x)
  x <- strsplit(x, "")
  count <- integer(length(u_pairs))
  names(count) <- u_pairs
  for (i in 1:n) {
    if (len[i] == 0) {
      count["AA"] <- count["AA"] + count_in[i]
      next
    }
    y <- x[[i]]
    p <- paste0("A", y[1])
    count[p] <- count[p] + count_in[i]
    if (len[i] >= 2) {
      for (j in 1:(len[i] - 1)) {
        p <- paste0(y[j], y[j + 1])
        count[p] <- count[p] + count_in[i]
      }
    }
    p <- paste0(y[len[i]], "A")
    count[p] <- count[p] + count_in[i]
  }
  count
}

complexity <- function(codes, opt_num, opt_dir, robots) {
  comp <- 0
  nm <- rownames(opt_dir)
  u_pairs <- apply(expand.grid(nm, nm), 1, paste0, collapse = "")
  for (code in codes) {
    code_num <- as.numeric(substr(code, 1, 3))
    code_vec <- strsplit(code, "")[[1]]
    m <- length(code_vec)
    lhs <- c("A", code_vec[-m])
    rhs <- c(code_vec)
    blocks <- opt_num[cbind(lhs, rhs)]
    count_in <- rep(1, m)
    for (i in seq_len(robots)) {
      count <- count_pairs(blocks, count_in, u_pairs)
      idx <- which(count > 0)
      blocks <- opt_dir[idx]
      count_in <- count[idx]
    }
    opt_len <- sum(count_in * (nchar(blocks) + 1))
    comp <- comp + opt_len * code_num
  }
  comp
}

opt_num <- optimal_sequences(keypad_num, banned_num, cost_dir)
opt_dir <- optimal_sequences(keypad_dir, banned_dir, cost_dir)
result1 <- complexity(data, opt_num, opt_dir, robots = 2)
result1

# Part 2
result2 <- complexity(data, opt_num, opt_dir, robots = 25)
formatC(result2, format = "f", digits = 0)
