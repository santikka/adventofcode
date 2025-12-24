# Data
data <- readLines("inputs/day06_input.txt")
n <- length(data)
ops <- strsplit(data[n], " ")[[1]]
ops <- ops[nzchar(ops)]
nums <- strsplit(data[-n], " ") |>
  lapply(function(x) as.numeric(x[nzchar(x)])) |>
  do.call(what = "rbind")

# Part 1
problems <- vapply(
  seq_len(ncol(nums)),
  function(i) {
    prob_str <- paste0(nums[, i], collapse = ops[i])
    eval(str2lang(prob_str))
  },
  numeric(1)
)
result1 <- sum(problems)
formatC(result1, format = "f", digits = 0)

# Part 2
ops <- strsplit(data[n], "")[[1]]
idx <- which(ops != " ")
start <- idx
end <- c(idx[-1] - 2, nchar(data[n]))
nums <- lapply(
  data[-n],
  function(x) substring(x, start, end)
)
result2 <- 0
for (i in seq_along(idx)) {
  tmp <- vapply(nums, "[[", character(1), i) |>
    strsplit("") |>
    do.call(what = "rbind")
  terms <- apply(
    tmp, 2, function(x) as.numeric(paste0(x[x != " "], collapse = ""))
  )
  problem <- paste0(terms, collapse = ops[idx[i]])
  result2 <- result2 + eval(str2lang(problem))
}
formatC(result2, format = "f", digits = 0)
