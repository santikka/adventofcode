# Data
data <- readLines("2024/inputs/day04_input.txt") |>
  strsplit(split = "") |>
  do.call(what = "rbind")

# Part 1
n <- ncol(data)
north <- cbind(0, 0:-3)
east <- cbind(0:3, 0)
south <- cbind(0, 0:3)
west <- cbind(0:-3, 0)
northeast <- cbind(0:3, 0:-3)
southeast <- cbind(0:3, 0:3)
southwest <- cbind(0:-3, 0:3)
northwest <- cbind(0:-3, 0:-3)
directions <- list(
  north, east, south, west, northeast, southeast, southwest, northwest
)
xmas <- 0L
for (i in 1:n) {
  for (j in 1:n) {
    pos <- cbind(rep(i, 4), rep(j, 4))
    for (d in directions) {
      idx <- pos + d
      if (all(idx >= 1 & idx <= n)) {
        txt <- paste0(data[idx], collapse = "")
        xmas <- xmas + 1L * (txt == "XMAS")
      }
    }
  }
}
result1 <- xmas
result1

# Part 2
diag1 <- cbind(c(-1, 0, 1), c(-1, 0, 1))
diag2 <- cbind(c(-1, 0, 1), c(1, 0, -1))
x_mas <- 0L
for (i in 1:n) {
  for (j in 1:n) {
    pos <- cbind(rep(i, 3), rep(j, 3))
    d1 <- pos + diag1
    d2 <- pos + diag2
    if (all(d1 >= 1 & d1 <= n & d2 >= 1 & d2 <= n)) {
      txt1 <- paste0(data[d1], collapse = "")
      txt2 <- paste0(data[d2], collapse = "")
      x_mas <- x_mas + 1L * (all(c(txt1, txt2) %in% c("MAS", "SAM")))
    }
  }
}
result2 <- x_mas
result2
