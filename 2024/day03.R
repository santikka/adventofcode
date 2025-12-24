# Data
data <- paste0(readLines("2024/inputs/day03_input.txt"), collapse = "")

# Part 1
mul_pattern <- "mul\\([1-9]{1}[0-9]{0,2},[1-9]{1}[0-9]{0,2}\\)"
mul_match <- gregexec(mul_pattern, data, perl = TRUE)
muls <- c(regmatches(data, mul_match)[[1L]])
nums <- strsplit(muls, "\\(|,|\\)")
left <- vapply(nums, function(x) as.integer(x[[2]]), integer(1))
right <- vapply(nums, function(x) as.integer(x[[3]]), integer(1))
result1 <- sum(left * right)
result1

# Part 2
do_pattern <- "do\\(\\)"
dont_pattern <- "don\\'t\\(\\)"
do_match <- c(0, gregexec(do_pattern, data, perl = TRUE)[[1]])
dont_match <- c(gregexec(dont_pattern, data, perl = TRUE)[[1]])
products <- rep(0, length(muls))
mul_idx <- c(mul_match[[1]])
for (i in seq_along(do_match)) {
  start <- do_match[i]
  end <- dont_match[dont_match > start][1]
  valid <- start < mul_idx & mul_idx < end
  if (any(valid)) {
    products[valid] <- left[valid] * right[valid]
  }
}
result2 <- sum(products)
result2
