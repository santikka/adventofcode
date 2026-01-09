# Data
data <- readLines("2023/inputs/day12_input.txt") |>
  strsplit(split = " ")
conditions <- vapply(data, "[[", character(1), 1) |>
  strsplit(split = "")
groups <- lapply(data, function(x) as.integer(strsplit(x[2], ",")[[1]]))

# Part 1
arrangements <- function(syms, groups, i, j, k, cache) {
  key <- c(i, j, k)
  if (!is.null(cache[[key]])) {
    return(cache[[key]])
  }
  g <- length(groups)
  s <- length(syms)
  group_val <- Inf
  if (j <= g) {
    group_val <- groups[j] + 1
  }
  if (i > s) {
    if (k != 1 && k != group_val) {
      return(0)
    }
    return(1 * ((j + 1 * (k == group_val)) == g + 1))
  }
  count <- 0
  sym <- syms[i]
  if (sym != "." && j <= g && k <= groups[j]) {
    count <- count + arrangements(syms, groups, i + 1, j, k + 1, cache)
  }
  l <- k == group_val
  if (sym != "#" && (k == 1 || l)) {
    count <- count + arrangements(syms, groups, i + 1, j + 1 * l, 1, cache)
  }
  cache[[key]] <- count
  return(count)
}

count_arrangements <- function(conditions, groups) {
  n <- length(groups)
  count <- numeric(n)
  for (i in seq_len(n)) {
    cache <- hashtab(type = "identical")
    count[i] <- arrangements(conditions[[i]], groups[[i]], 1, 1, 1, cache)
  }
  sum(count)
}

result1 <- count_arrangements(conditions, groups)
result1

# Part 2
conditions_unfolded <- vapply(data, "[[", character(1), 1) |>
  vapply(function(x) paste0(rep(x, 5), collapse = "?"), character(1)) |>
  strsplit(split = "")
groups_unfolded <- lapply(groups, rep, times = 5)
result2 <- count_arrangements(conditions_unfolded, groups_unfolded)
formatC(result2, format = "f", digits = 0)
