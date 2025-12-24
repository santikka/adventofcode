# Data
data <- readLines("2024/inputs/day01_input.txt") |>
  strsplit(split = "   ")
left <- vapply(data, function(x) as.integer(x[[1]]), integer(1))
right <- vapply(data, function(x) as.integer(x[[2]]), integer(1))

# Part 1
result1 <- sum(abs(sort(left) - sort(right)))
result1

# Part 2
included <- right[right %in% left]
tab <- table(included)
result2 <- sum(as.integer(names(tab)) * tab)
result2
