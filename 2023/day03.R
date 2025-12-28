# Data
data <- readLines("2023/inputs/day03_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind") |>
  rbind(".", ... = _, ".") |>
  cbind(".", ... = _, ".")

# Part 1
part_numbers <- function(schematic) {
  nums <- which(schematic %in% as.character(0:9)) |>
    arrayInd(.dim = dim(schematic))
  syms <- which(!schematic %in% c(as.character(0:9), ".")) |>
    arrayInd(.dim = dim(schematic))
  idx <- as.matrix(expand.grid(c(0, -1, 1), c(0, -1, 1))[-1, ])
  valid <- lapply(1:nrow(syms), function(i) sweep(idx, 2, syms[i, ], "+")) |>
    do.call(what = "rbind") |>
    unique()
  out <- 0
  for (i in 2:(nrow(schematic) - 1)) {
    valid_i <- valid[valid[, 1] == i, 2]
    nums_i <- nums[nums[, 1] == i, 2]
    if (length(valid_i) == 0 || length(nums_i) == 0) {
      next
    }
    m <- length(nums_i)
    d <- c(1, which(diff(nums_i) != 1) + 1, m + 1)
    part_no <- vapply(
      1:(length(d) - 1),
      function(j) {
        idx <- d[j]:(d[j + 1] - 1)
        if (any(nums_i[idx] %in% valid_i)) {
          as.integer(paste0(schematic[i, nums_i[idx]], collapse = ""))
        } else {
          0L
        }
      },
      integer(1)
    )
    out <- out + sum(part_no)
  }
  out
}

result1 <- part_numbers(data)
result1

# Part 2
gear_ratios <- function(schematic) {
  nums <- which(schematic %in% as.character(0:9)) |>
    arrayInd(.dim = dim(schematic))
  asts <- which(schematic == "*", arr.ind = TRUE)
  idx <- as.matrix(expand.grid(c(0, -1, 1), c(0, -1, 1))[-1, ])
  valid <- lapply(1:nrow(asts), function(i) sweep(idx, 2, asts[i, ], "+"))
  out <- 0
  for (i in 1:length(valid)) {
    ast_i <- valid[[i]]
    u <- unique(ast_i[, 1])
    part_no <- integer(0)
    for (j in 1:length(u)) {
      valid_j <- ast_i[ast_i[, 1] == u[j], 2]
      nums_j <- nums[nums[, 1] == u[j], 2]
      if (length(nums_j) == 0) {
        next
      }
      m <- length(nums_j)
      d <- c(1, which(diff(nums_j) != 1) + 1, m + 1)
      part_no_new <- vapply(
        1:(length(d) - 1),
        function(k) {
          idx <- d[k]:(d[k + 1] - 1)
          if (any(nums_j[idx] %in% valid_j)) {
            as.integer(paste0(schematic[u[j], nums_j[idx]], collapse = ""))
          } else {
            0L
          }
        },
        integer(1)
      )
      part_no <- c(part_no, part_no_new[part_no_new != 0L])
    }
    out <- out + (length(part_no) == 2) * prod(part_no)
  }
  out
}

result2 <- gear_ratios(data)
result2
