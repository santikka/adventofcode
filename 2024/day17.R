# install.packages("bigBits")
library("R6")

# Data
data <- readLines("2024/inputs/day17_input.txt") |>
  strsplit(": ")
reg_a <- as.integer(data[[1]][2])
reg_b <- as.integer(data[[2]][2])
reg_c <- as.integer(data[[3]][2])
program <- as.integer(strsplit(data[[5]][2], ",")[[1]])

# Part 1
bitwise_xor <- function(x, y) {
  max_int <- .Machine$integer.max
  if (x > max_int || y > max_int) {
    bigBits::bigXor(x, y)
  } else {
    bitwXor(x, y)
  }
}

ChronospatialComputer <- R6Class(
  "ChronospatialComputer",
  public = list(
    initialize = function(program, reg_a, reg_b, reg_c) {
      private$reg_a <- reg_a
      private$reg_b <- reg_b
      private$reg_c <- reg_c
      private$program <- program
      private$instr_ptr <- 1
      private$null_ptr <- length(program) + 1
    },
    run_program = function() {
      while (private$instr_ptr != private$null_ptr) {
        instruction <- private$program[private$instr_ptr] + 1
        operand <- private$program[private$instr_ptr + 1]
        self$run_instruction(instruction, operand)
      }
    },
    run_instruction = function(index, operand) {
      private[[self$get_instruction(index)]](operand)
    },
    get_instruction = function(index) {
      switch(index,
        `1` = "adv",
        `2` = "bxl",
        `3` = "bst",
        `4` = "jnz",
        `5` = "bxc",
        `6` = "out",
        `7` = "bdv",
        `8` = "cdv"
      )
    },
    get_output = function() {
      private$output
    },
    reset = function() {
      private$output <- NULL
      private$reg_a <- 0
      private$reg_b <- 0
      private$reg_c <- 0
      private$instr_ptr <- 1
    },
    set_register = function(value) {
      private$reg_a <- value
    },
    try_combo = function(op) {
      print(private$get_combo(op))
    }
  ),
  private = list(
    adv = function(op) {
      combo <- private$get_combo(op)
      private$reg_a <- private$reg_a %/% 2^combo
      private$instr_ptr <- private$instr_ptr + 2
    },
    bxl = function(op) {
      private$reg_b <- bitwise_xor(private$reg_b, op)
      private$instr_ptr <- private$instr_ptr + 2
    },
    bst = function(op) {
      combo <- private$get_combo(op)
      private$reg_b <- combo %% 8
      private$instr_ptr <- private$instr_ptr + 2
    },
    jnz = function(op) {
      if (private$reg_a != 0) {
        private$instr_ptr <- op + 1
      } else {
        private$instr_ptr <- private$instr_ptr + 2
      }
    },
    bxc = function(op) {
      private$reg_b <- bitwise_xor(private$reg_b, private$reg_c)
      private$instr_ptr <- private$instr_ptr + 2
    },
    out = function(op) {
      combo <- private$get_combo(op)
      private$write(combo %% 8)
      private$instr_ptr <- private$instr_ptr + 2
    },
    bdv = function(op) {
      combo <- private$get_combo(op)
      private$reg_b <- private$reg_a %/% 2^combo
      private$instr_ptr <- private$instr_ptr + 2
    },
    cdv = function(op) {
      combo <- private$get_combo(op)
      private$reg_c <- private$reg_a %/% 2^combo
      private$instr_ptr <- private$instr_ptr + 2
    },
    get_combo = function(op) {
      (op %in% 0:3) * op + 
        (op == 4) * private$reg_a +
        (op == 5) * private$reg_b +
        (op == 6) * private$reg_c
    },
    write = function(value) {
      if (is.null(private$output)) {
        private$output <- value
      } else {
        private$output <- paste0(private$output, ",", value)
      }
    },
    output = NULL,
    program = NULL,
    reg_a = NULL,
    reg_b = NULL,
    reg_c = NULL,
    instr_ptr = NULL,
    null_ptr = NULL
  )
)

comp <- ChronospatialComputer$new(program, reg_a, reg_b, reg_c)
comp$run_program()
result1 <- comp$get_output()
result1

# Part 2
computer <- function(a) {
  out <- c()
  if (a == 0) {
    return(1)
  }
  while (a != 0) {
    b <- bitwise_xor(a %% 8, 2)
    b <- bitwise_xor(b, a %/% 2^b)
    b <- bitwise_xor(b, 3)
    out <- c(out, b %% 8)
    a <- a %/% 8
  }
  out
}

invert_computer <- function(reg_a, program, i) {
  if (i == 0) {
    return(reg_a)
  }
  out <- double(8)
  n <- length(program)
  for (a in 0:7) {
    new_reg <- reg_a * 8 + a
    if (all(computer(new_reg) == program[i:n])) {
      tmp <- invert_computer(new_reg, program, i - 1)
      if (tmp > 0) {
        return(tmp)
      }
      out[a + 1] <- tmp
    }
  }
  return(0)
}

result2 <- invert_computer(0, program, length(program))
formatC(result2, format = "f", digits = 0)
