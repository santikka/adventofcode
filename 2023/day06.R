# Data
data <- readLines("2023/inputs/day06_input.txt")
time <- strsplit(data[1], " +")[[1]][-1] |>
  as.integer()
distance <- strsplit(data[2], " +")[[1]][-1] |>
  as.integer()

# Part 1
beat_records <- function(time, distance) {
  n <- length(time)
  count <- integer(n)
  for (i in seq_len(n)) {
    roots <- 0.5 * time[i] + c(-0.5, 0.5) * sqrt(time[i]^2 - 4 * distance[i])
    count[i] <- ceiling(roots[2] - 1) - floor(roots[1] + 1) + 1
  }
  prod(count)
}

result1 <- beat_records(time, distance)
result1


# Part 2
single_time <- as.numeric(paste0(time, collapse = ""))
single_distance <- as.numeric(paste0(distance, collapse = ""))
result2 <- beat_records(single_time, single_distance)
result2
