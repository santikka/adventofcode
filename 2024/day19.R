# Data
data <- readLines("2024/inputs/day19_input.txt")
towels <- strsplit(data[1], ", ")[[1]]
designs <- data[-(1:2)]

# Part 1
pattern <- paste0("^(", paste0(towels, collapse = "|"), ")+$")
possible <- grepl(pattern, designs, perl = TRUE)
result1 <- sum(possible)
result1

# Part 2
count_unique <- function(towels, design, cache) {
  d_len <- nchar(design)
  if (d_len == 0) {
    return(1)
  }
  if (!is.null(cache[[design]])) {
    return(cache[[design]])
  }
  len <- nchar(towels)
  lo <- min(len)
  hi <- min(max(len), d_len)
  count <- 0
  for (i in lo:hi) {
    prefix <- substring(design, 1, i)
    if (prefix %in% towels[len == i]) {
      suffix <- substring(design, i + 1, d_len)
      suffix_count <- count_unique(towels, suffix, cache)
      if (i + 1 <= d_len) {
        cache[[suffix]] <- suffix_count
      }
      count <- count + suffix_count
    }
  }
  count
}

unique_arrangements <- function(towels, designs, possible) {
  cache <- new.env()
  viable <- designs[possible]
  count <- 0
  for (design in viable) {
     count <- count + count_unique(towels, design, cache)
  }
  count
}

result2  <- unique_arrangements(towels, designs, possible)
formatC(result2, format = "f", digits = 0)
