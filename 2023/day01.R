# Data
data <- readLines("2023/inputs/day01_input.txt")

# Part 1
calibration_values <- function(data) {
  data <- strsplit(data, split = "")
  values <- integer(length(data))
  digits <- as.character(0:9)
  for (i in seq_along(data)) {
    d <- data[[i]]
    nums <- d[d %in% digits]
    n <- length(nums)
    values[i] <- as.integer(paste0(nums[1], nums[n]))
  }
  values
}

result1 <- sum(calibration_values(data))
result1

# Part 2
calibration_values_corrected <- function(data) {
  pat <- "one|two|three|four|five|six|seven|eight|nine|[0-9]"
  match <- gregexec(pat, data)
  nums <- regmatches(data, match)
  values <- integer(length(nums))
  for (i in seq_along(nums)) {
    d <- c(nums[[i]])
    d[d == "one"] <- "1"
    d[d == "two"] <- "2"
    d[d == "three"] <- "3"
    d[d == "four"] <- "4"
    d[d == "five"] <- "5"
    d[d == "six"] <- "6"
    d[d == "seven"] <- "7"
    d[d == "eight"] <- "8"
    d[d == "nine"] <- "9"
    n <- length(d)
    values[i] <- as.integer(paste0(d[1], d[n]))
  }
  values
}

result2 <- sum(calibration_values_corrected(data))
result2
