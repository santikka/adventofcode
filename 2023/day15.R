# Data
data <- readLines("2023/inputs/day15_input.txt") |>
  strsplit(split = ",") |>
  getElement(1)

to_ascii <- function(x) {
  max_len <- max(nchar(x))
  x |>
    lapply(
      function(y) {
        z <- utf8ToInt(y)
        length(z) <- max_len
        z
      }
    ) |>
    do.call(what = "rbind")
}

ascii_steps <- to_ascii(data)
steps <- data |>
  strsplit(split = "-|=")
labels <- vapply(steps, "[[", character(1), 1)
ascii_labels <- to_ascii(labels)

# Part 1
compute_hash <- function(ascii) {
  n <- nrow(ascii)
  m <- ncol(ascii)
  out <- integer(n)
  for (i in 1:m) {
    idx <- !is.na(ascii[, i])
    out[idx] <- (17 * (out[idx] + ascii[idx, i])) %% 256
  }
  out
}

result1 <- sum(compute_hash(ascii_steps))
result1

# Part 2
arrange_lenses <- function(steps, labels, ascii) {
  hash <- compute_hash(ascii) + 1
  boxes <- vector(mode = "list", length = max(hash))
  len <- lengths(steps)
  add <- len == 2
  for (i in seq_along(steps)) {
    h <- hash[i]
    lab <- labels[i]
    if (add[i]) {
      boxes[[h]][lab] <- as.integer(steps[[i]][2])
    } else {
      idx <- which(names(boxes[[h]]) == lab)
      if (length(idx) > 0) {
        boxes[[h]] <- boxes[[h]][-idx]
      }
    }
  }
  focal <- vapply(
    seq_along(boxes),
    function(i) {
      j <- seq_along(boxes[[i]])
      i * sum(j * boxes[[i]])
    },
    integer(1)
  )
  sum(focal)
}

result2 <- arrange_lenses(steps, labels, ascii_labels)
result2
