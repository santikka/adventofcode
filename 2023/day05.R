# Data
data <- readLines("2023/inputs/day05_input.txt")
seeds <- as.numeric(strsplit(data[1], " ")[[1]][-1])
idx <- c(which(nchar(data) == 0), length(data) + 1)
maps <- lapply(
  1:(length(idx) - 1), 
  function(i) data[(idx[i] + 2):(idx[i + 1] - 1)]
)
maps <- lapply(
  maps, 
  function(x) {
    y <- strsplit(x, " ") |>
      lapply(as.numeric) |>
      do.call(what = "rbind")
    y[order(y[, 2]), c(2, 1, 3)]
  }
)

# Part 1
get_location <- function(seeds, maps) {
  min_loc <- Inf
  for (seed in seeds) {
    source <- seed
    for (map in maps) {
      for (i in 1:nrow(map)) {
        source_start <- map[i, 1]
        source_end <- source_start + map[i, 3] - 1
        dest_start <- map[i, 2]
        if (source_start <= source && source <= source_end) {
          dist <- source - source_start
          source <- dest_start + dist
          break
        }
      }
    }
    min_loc <- min(min_loc, source)
  }
  min_loc
}

result1 <- get_location(seeds, maps)
result1

# Part 2
process_maps <- function(range, maps) {
  range_start <- range[1]
  if (length(maps) == 0) {
    return(range_start)
  }
  range_end <- range_start + range[2] - 1
  map <- maps[[1]]
  n <- nrow(map)
  min_loc <- Inf
  source_start <- map[1, 1]
  if (range_start < source_start) {
    new_range <- c(range_start, source_start - range_start)
    new_loc <- process_maps(new_range, maps[-1])
    min_loc <- min(min_loc, new_loc)
    range_start <- source_start
  }
  for (i in 1:n) {
    source_start <- map[i, 1]
    source_end <- source_start + map[i, 3] - 1
    dest_start <- map[i, 2]
    if (range_end < source_start) {
      break
    }
    if (source_end < range_start) {
      next
    }
    offset <- range_start - source_start
    min_end <- min(range_end, source_end)
    new_range <- c(dest_start + offset, min_end - range_start + 1)
    new_loc <- process_maps(new_range, maps[-1])
    min_loc <- min(min_loc, new_loc)
    range_start <- min_end + 1
    if (range_start > range_end) {
      break
    }
  }
  if (range_end > source_end) {
    new_loc <- process_maps(range, maps[-1])
    min_loc <- min(min_loc, new_loc)
  }
  min_loc
}

get_location_ranges <- function(seed_ranges, maps) {
  n <- nrow(seed_ranges)
  min_loc <- Inf
  for (i in 1:n) {
    new_loc <- process_maps(seed_ranges[i, ], maps)
    min_loc <- min(min_loc, new_loc)
  }
  min_loc
}

seed_ranges <- matrix(seeds, ncol = 2, byrow = TRUE)
result2 <- get_location_ranges(seed_ranges, maps)
result2
