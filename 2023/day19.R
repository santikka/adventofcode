# Data
data <- readLines("2023/inputs/day19_input.txt")
idx <- which(nchar(data) == 0)
workflows <- data[1:(idx - 1)]
inputs <- data[(idx + 1):length(data)]
numbers_reg <- gregexec("[0-9]+", inputs, perl = TRUE)
numbers <- regmatches(inputs, numbers_reg) |>
  lapply(as.numeric) |>
  do.call(what = "rbind")

# Part 1
construct_functions <- function(workflows, e) {
  n <- length(workflows)
  tmp <- strsplit(workflows, "\\{|\\}")
  nm <- vapply(tmp, "[[", character(1), 1)
  body <- vapply(tmp, "[[", character(1), 2)
  for (i in seq_len(n)) {
    conds <- strsplit(body[i], ",")[[1]]
    fun_str <- "function(x, m, a, s) {\n"
    for (j in seq_along(conds)) {
      cond <- strsplit(conds[j], ":")[[1]]
      if (length(cond) > 1) {
        if_prefix <- paste0("if (", cond[1], ") {\n")
        if_suffix <- "}\n"
        cond <- cond[2]
      } else {
        if_prefix <- ""
        if_suffix <- ""
      }
      return_prefix <- "return("
      if (cond == "A") {
        return_val <- "sum(x, m, a, s)"
      } else if (cond == "R") {
        return_val <- "0"
      } else {
        return_val <- paste0("e[['", cond, "']](x, m, a, s)")
      }
      fun_str <- paste0(
        fun_str, if_prefix, return_prefix, return_val, ")\n", if_suffix
      )
    }
    fun_str <- paste0(fun_str, "}")
    e[[nm[i]]] <- eval(str2lang(fun_str))
  }
}

get_ratings <- function(workflows, numbers) {
  e <- new.env()
  construct_functions(workflows, e)
  n <- nrow(numbers)
  out <- 0
  for (i in seq_len(n)) {
    x <- numbers[i, 1]
    m <- numbers[i, 2]
    a <- numbers[i, 3]
    s <- numbers[i, 4]
    out <- out + e[["in"]](x, m, a, s)
  }
  out
}

result1 <- get_ratings(workflows, numbers)
result1

# Part 2
construct_range_functions <- function(workflows, e) {
  n <- length(workflows)
  tmp <- strsplit(workflows, "\\{|\\}")
  nm <- vapply(tmp, "[[", character(1), 1)
  body <- vapply(tmp, "[[", character(1), 2)
  for (i in seq_len(n)) {
    conds <- strsplit(body[i], ",")[[1]]
    fun_str <- "function(ratings) {\n"
    body_str <- paste0(
      "rating_list <- vector(mode = 'list', length = ", length(conds), ")\n",
      "new_ratings <- ratings\n"
    )
    n <- length(conds)
    for (j in seq_len(n)) {
      cond <- strsplit(conds[j], ":")[[1]]
      next_val <- ""
      next_ratings <- ""
      new_val <- ""
      new_ratings <- ""
      return_val <- ""
      call_ratings <- ifelse(j < n, "next_ratings", "new_ratings")
      if (length(cond) > 1) {
        rating <- substr(cond[1], 1, 1)
        op <- substr(cond[1], 2, 2)
        val <- as.integer(substr(cond[1], 3, 6))
        dir <- op == ">"
        col <- ifelse(dir, 1, 2)
        offset <- ifelse(dir, 1, -1)
        fun <- ifelse(dir, "max", "min")
        rev_fun <- c(max = "min", min = "max")[fun]
        rev_col <- c(2, 1)[col]
        next_val <- paste0(
          "next_val <- ", fun, 
          "(new_ratings['", rating, "', ", col, "], ", 
          val + offset, ")\n"
        )
        next_ratings <- paste0(
          "next_ratings <- new_ratings\n",
          "next_ratings['", rating, "', ", col, "] <- next_val\n"
        )
        new_val <- paste0(
          "new_val <- ", rev_fun, 
          "(new_ratings['", rating, "', ", rev_col, "], ",
          val, ")\n"
        )
        new_ratings <- paste0(
          "new_ratings['", rating, "', ", rev_col, "] <- new_val\n"
        )
        cond <- cond[2]
      }
      if (cond == "A") {
        return_val <- call_ratings
      } else if (cond == "R") {
        return_val <- "list()"
      } else {
        return_val <- paste0("e[['", cond, "']](", call_ratings, ")\n")
      }
      output <- paste0("rating_list[[", j, "]] <- ", return_val, "\n")
      body_str <- paste0(
        body_str,
        next_val,
        next_ratings,
        new_val,
        new_ratings,
        output
      )
    }
    fun_str <- paste0(fun_str, body_str, "return(rating_list)\n}")
    e[[nm[i]]] <- eval(str2lang(fun_str))
  }
}

get_combinations <- function(workflows) {
  count_combinations <- function(ratings) {
    prod(ratings[, 2] - ratings[, 1] + 1)
  }
  e <- new.env()
  construct_range_functions(workflows, e)
  ratings <- matrix(rep(c(1, 4000), each = 4), 4, 2)
  rownames(ratings) <- c("x", "m", "a", "s")
  out <- e[["in"]](ratings)
  sum(rapply(out, count_combinations))
}

result2 <- get_combinations(workflows)
formatC(result2, format = "f", digits = 0)
