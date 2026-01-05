# Data
data <- readLines("2023/inputs/day18_input.txt") |>
  strsplit(split = " ")
directions <- vapply(data, "[[", character(1), 1)
numbers <- vapply(data, "[[", character(1), 2) |>
  as.numeric()
hexa <-  vapply(data, "[[", character(1), 3) |>
  gsub(pattern = "\\(|\\)|#", replacement = "", perl = TRUE)
direction_nums <- c(`0` = "R", `1` = "D", `2` = "L", `3` = "U")
hexa_directions <- direction_nums[substr(hexa, 6, 6)]
hexa_numbers <- substr(hexa, 1, 5) |>
  as.hexmode() |>
  as.numeric()

# Part 1
total_area <- function(dirs, nums) {
  n <- length(nums)
  moves <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), ncol = 2)
  rownames(moves) <- c("L", "R", "D", "U")
  coord <- c(0, 0)
  area <- 0
  for (i in seq_along(dirs)) {
    move <- moves[dirs[i], ]
    prev <- coord
    coord <- coord + nums[i] * move
    area <- area + prev[1] * coord[2] - coord[1] * prev[2]
  }
  0.5 * abs(area) + 0.5 * sum(nums) + 1
}

result1 <- total_area(directions, numbers)
result1

# Part 2
result2 <- total_area(hexa_directions, hexa_numbers)
formatC(result2, format = "f", digits = 0)
