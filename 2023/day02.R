# Data
data <- readLines("2023/inputs/day02_input.txt") |>
  strsplit(split = ":|;") |>
  lapply(trimws)

extract_colors <- function(data, color) {
  pat <- paste0("([1-9][0-9]{0,1}) ", color)
  balls <- lapply(
    data,
    function(x) {
      match <- gregexec(pat, x[-1])
      out <- regmatches(x[-1], match)
      vapply(
        out, 
        function(y) {
          if (length(y) > 0) {
            as.integer(y[2, ])
          } else {
            0L
          }
        },
        integer(1)
      )
    }
  )
}

red <- extract_colors(data, "red")
green <- extract_colors(data, "green")
blue <- extract_colors(data, "blue")

# Part 1
possible_games <- function(red, green, blue) {
  n <- length(red)
  possible <- logical(n)
  for (i in seq_along(red)) {
    r <- red[[i]]
    g <- green[[i]]
    b <- blue[[i]]
    possible[i] <- all(r <= 12 & g <= 13 & b <= 14)
  }
  possible
}

result1 <- sum(which(possible_games(red, green, blue)))
result1

# Part 2
minimum_power <- function(red, green, blue) {
  n <- length(red)
  power <- integer(n)
  for (i in seq_along(red)) {
    r <- red[[i]]
    g <- green[[i]]
    b <- blue[[i]]
    power[i] <- max(r) * max(g) * max(b)
  }
  power
}

result2 <- sum(minimum_power(red, green, blue))
result2
